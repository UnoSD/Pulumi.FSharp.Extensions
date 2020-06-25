# Pulumi.FSharp.Azure
F# computational expressions to reduce boilerplate in Pulumi code

[![NuGet Version and Downloads count](https://buildstats.info/nuget/Pulumi.FSharp.Azure)](https://www.nuget.org/packages/Pulumi.FSharp.Azure)

# Example usage

```fsharp
open Pulumi.FSharp.Azure

let rg =
    resourceGroup {
        name                "ResourceGroupName"
    }

let sa =
    storageAccount {
        name                "StorageAccountName"
        resourceGroup       rg
        replication         LRS
        tier                Standard
        httpsOnly           true
    }
    
let container =
    storageContainer {
        name                "StorageContainer"
        storageAccountInput sa.Name
        access              Private
        containerName       "containername"
    }
    
let blob =
    storageBlob {
        name                "StorageBlob"
        storageAccount      storage
        container           buildContainer
        source              (input (("Blob content" |> StringAsset) :> AssetOrArchive))
    }
    
let appServicePlan =
    appService {
        name                "FunctionAppServiceName"
        resourceGroup       rg
    }
    
let appInsights =
    appInsight {
        name                "AppInsight"
        resourceGroup       rg
        applicationType     Web
        retentionInDays     90
    }
```