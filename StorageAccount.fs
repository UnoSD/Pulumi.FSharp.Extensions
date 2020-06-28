// Autogenerate the builders based on Pulumi assembly

namespace Pulumi.FSharp.Azure

open Pulumi.FSharp.Azure.Regions
open System.Collections.Generic
open Pulumi.FSharp.Azure.Core
open Pulumi.Azure.Storage
open Pulumi.Azure.Core
open Pulumi.FSharp
open Pulumi

[<AutoOpen>]
module StorageAccount =    
    type Replication =
        | LRS
        
    type Tier =
        | Standard

    type StorageAccountArgsRecord = {
        ResourceGroup: IOArg<ResourceGroup>
        Replication: Replication
        Tier: Tier
        HttpsOnly: bool
    }

    type StorageAccountBuilder internal () =
        inherit AzureResource()
        
        member __.Yield _ = (AzureResource.Zero, {
            ResourceGroup = Name ""
            Replication = LRS
            Tier = Standard
            HttpsOnly = true
        })

        member __.Run (cargs, args) =
            cargs.Region |>
            regionName |>
            input |>
            (fun l  -> AccountArgs(Location = l,
                                   ResourceGroupName = (getName args.ResourceGroup),
                                   // Convert from type name to string
                                   AccountTier = input (match args.Tier with | Standard -> "Standard"),
                                   AccountReplicationType = input (match args.Replication with | LRS -> "LRS"),
                                   EnableHttpsTrafficOnly = input args.HttpsOnly,
                                   Tags = inputMap cargs.Tags)) |>
            fun saa -> Account(cargs.Name,
                               saa,
                               CustomResourceOptions(AdditionalSecretOutputs = List<string>([
                                   "PrimaryAccessKey"
                                   "SecondaryAccessKey"
                                   "PrimaryConnectionString"
                                   "PrimaryBlobConnectionString"
                                   "SecondaryConnectionString"
                                   "SecondaryBlobConnectionString"
                               ])))

        [<CustomOperation("replication")>]
        member __.Replication((cargs, args : StorageAccountArgsRecord), replication) = cargs, { args with Replication = replication }
        
        [<CustomOperation("tier")>]
        member __.Tier((cargs, args : StorageAccountArgsRecord), tier) = cargs, { args with Tier = tier }
        
        [<CustomOperation("httpsOnly")>]
        member __.HttpsOnly((cargs, args : StorageAccountArgsRecord), httpsOnly) = cargs, { args with HttpsOnly = httpsOnly }
        
        [<CustomOperation("resourceGroup")>]
        member __.ResourceGroup((cargs, args : StorageAccountArgsRecord), resourceGroup) = cargs, {
            args with ResourceGroup = Object resourceGroup
        }
        
        member __.ResourceGroup((cargs, args : StorageAccountArgsRecord), resourceGroup) = cargs, {
            args with ResourceGroup = Name resourceGroup
        }

    let storageAccount = StorageAccountBuilder()