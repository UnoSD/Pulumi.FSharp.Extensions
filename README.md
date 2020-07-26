# Pulumi.FSharp.Extensions
F# computational expressions to reduce boilerplate in Pulumi code

[![NuGet Version and Downloads count](https://buildstats.info/nuget/Pulumi.FSharp.Azure)](https://www.nuget.org/packages/Pulumi.FSharp.Azure)
[![NuGet Version and Downloads count](https://buildstats.info/nuget/Pulumi.FSharp.Aws)](https://www.nuget.org/packages/Pulumi.FSharp.Aws)
[![NuGet Version and Downloads count](https://buildstats.info/nuget/Pulumi.FSharp.Kubernetes)](https://www.nuget.org/packages/Pulumi.FSharp.Kubernetes)
[![NuGet Version and Downloads count](https://buildstats.info/nuget/Pulumi.FSharp.Core)](https://www.nuget.org/packages/Pulumi.FSharp.Core)

# Pulumi.FSharp.Azure

# Example usage

Create a Pulumi F# project using `pulumi new azure-fsharp` and add the NuGet packages above.

Edit the Program.fs similarly to the example below.

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