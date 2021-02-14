# Pulumi.FSharp.Extensions
F# computational expressions to reduce boilerplate in Pulumi code

[![NuGet Version and Downloads count](https://buildstats.info/nuget/Pulumi.FSharp.Azure)](https://www.nuget.org/packages/Pulumi.FSharp.Azure)
[![NuGet Version and Downloads count](https://buildstats.info/nuget/Pulumi.FSharp.AzureAD)](https://www.nuget.org/packages/Pulumi.FSharp.AzureAD)
[![NuGet Version and Downloads count](https://buildstats.info/nuget/Pulumi.FSharp.Aws)](https://www.nuget.org/packages/Pulumi.FSharp.Aws)
[![NuGet Version and Downloads count](https://buildstats.info/nuget/Pulumi.FSharp.Kubernetes)](https://www.nuget.org/packages/Pulumi.FSharp.Kubernetes)
[![NuGet Version and Downloads count](https://buildstats.info/nuget/Pulumi.FSharp.Core)](https://www.nuget.org/packages/Pulumi.FSharp.Core)

# Pulumi.FSharp.Azure

```fsharp
open Pulumi.FSharp.Azure

let rg =
    resourceGroup {
        name            "ResourceGroupName"
    }

let sa =
    storageAccount {
        name            "StorageAccountName"
        resourceGroup   rg
        replication     LRS
        tier            Standard
        httpsOnly       true
    }
    
let container =
    storageContainer {
        name            "StorageContainer"
        account         sa.Name
        access          Private
        containerName   "containername"
    }
    
let blob =
    storageBlob {
        name            "StorageBlob"
        account         storage
        container       buildContainer
        source          (input (("Blob content" |> StringAsset) :> AssetOrArchive))
    }
    
let appServicePlan =
    appService {
        name            "FunctionAppServiceName"
        resourceGroup   rg
    }
    
let appInsights =
    appInsight {
        name            "AppInsight"
        resourceGroup   rg
        applicationType Web
        retentionInDays 90
    }
    
let template =
    armTemplate {
        name            "ArmTemplate"
        resourceGroup   rg
        json            (File.ReadAllText("Template.json"))
        parameters      [ "location", io rg.Location ]
    }
    
let sasToken =
    sasToken {
        storage         sa
        blob            apiBlob
    }
    
// Output computational expressions
let deploymentCountBars =
    output {
        let! previousOutputs =
            StackReference(Deployment.Instance.StackName).Outputs
        
        return previousOutputs.["CountBars"] + "I"
    }
    
// Output as secret
let someSecret =
    secretOutput {
        let! key1 = sa.PrimaryConnectionString
        let! key2 = sa.SecondaryConnectionString
        
        return "Secret connection strings: " + key1 + " " + key2
    }
    
// Mixing Output<> and Task<>
let sas =
    output {
        let! connectionString = sa.PrimaryConnectionString
        let! containerName = container.Name
        let! url = blob.Url

        let start =
            DateTime.Now.ToString("u").Replace(' ', 'T')
        
        let expiry =
            DateTime.Now.AddHours(1.).ToString("u").Replace(' ', 'T')
        
        // Task to Output
        let! tokenResult =
            GetAccountBlobContainerSASArgs(
                ConnectionString = connectionString,
                ContainerName = containerName,
                Start = start,
                Expiry = expiry,
                Permissions = (GetAccountBlobContainerSASPermissionsArgs(Read = true))
            ) |>
            // This returns Task<>
            GetAccountBlobContainerSAS.InvokeAsync

        return url + tokenResult.Sas
    }
```

# Full stack file example

Create a Pulumi F# project using `pulumi new azure-fsharp` and add the NuGet packages above.

Edit the `Program.fs` similarly to the example below.

Run `pulumi up` and create the infrastructure using a readable strongly-typed DSL.

To discover available properties for each resource, examine the code documentation of the builders (E.G. hover over a `Pulumi.FSharp.Azure.Compute.windowsVirtualMachine` computational expression)

```fsharp
module Program

open Pulumi.FSharp.Azure.Compute.Inputs
open Pulumi.FSharp.Azure.Compute
open Pulumi.FSharp.Config
open Pulumi.FSharp.Output
open Pulumi.FSharp

let infra () =
    let vm =
        windowsVirtualMachine {
            name "PulumiVmName"
            resourceName "AzureNameForVm"
            
            resourceGroup "ResourceGroupName"
            networkInterfaceIds [ config.["nicResourceIdFromConfig"] ]
            size "Standard_D4s_v3"
            
            windowsVirtualMachineOsDisk {
                name config.["vmDiskName"]
                caching "ReadWrite"
                storageAccountType "Standard_LRS"
            }
            
            adminUsername config.["vmUser"]
            adminPassword secret.["encryptedPasswordFromPulumiConfig"]
            
            windowsVirtualMachineSourceImageReference {
                offer "visualstudio2019latest"
                publisher "microsoftvisualstudio"
                sku "vs-2019-comm-latest-win10-n"
                version "latest"
            }
        }
    
    let secretValue =
        secretOutput {
            return vm.PublicIpAddress
        }
    
    let pipCird =
        output {
            let! pip = vm.PublicIpAddress
            
            return pip + "/32"
        }
    
    dict [ "SecretPublicIP",      secretValue :> obj
           "VisiblePublicIPCIDR", pipCird     :> obj ]
           
[<EntryPoint>]
let main _ =
  Deployment.run infra
```

# One language to rule them all (F# eXchange 2020)

I had the pleasure of being invited to speak at the F# eXchange 2020 on the 21st of October and, as promised, I have created a repo to share the slides of the talk: https://github.com/UnoSD/FSharp-eXChange2020

The talk goes through infrastructure as code in F# with Pulumi in general and explains the rationale and some of the technical details behind this repository.

Link to the video: https://skillsmatter.com/skillscasts/14888-lightning-talk-one-language-to-rule-them-all-iac-in-f-sharp

# Development notes

Given that changes to the `Pulumi.FSharp.Myriad` project do not trigger re-generation unless the Myriad.fs file is modified, a `PreBuildEvent` in each `fsproj` file uses `sed` to change a dummy variable to a random integer on each build (this only works on GNU/Linux machines with `sed`). To avoid `git` finding changes in the `Myriad.fs` file every time, use the following:

```bash
git update-index --skip-worktree Pulumi.FSharp.Aws/Myriad.fs
git update-index --skip-worktree Pulumi.FSharp.Azure/Myriad.fs
git update-index --skip-worktree Pulumi.FSharp.AzureAD/Myriad.fs
git update-index --skip-worktree Pulumi.FSharp.Kubernetes/Myriad.fs
```