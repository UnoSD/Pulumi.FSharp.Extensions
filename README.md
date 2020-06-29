# Pulumi.FSharp.Azure
F# computational expressions to reduce boilerplate in Pulumi code

[![NuGet Version and Downloads count](https://buildstats.info/nuget/Pulumi.FSharp.Azure)](https://www.nuget.org/packages/Pulumi.FSharp.Azure)

# Example usage

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