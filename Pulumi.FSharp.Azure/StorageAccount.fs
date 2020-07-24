// Autogenerate the builders based on Pulumi assembly
[<AutoOpen>]
module Pulumi.FSharp.Azure.Legacy.StorageAccount

open Pulumi.FSharp.Azure.Regions
open System.Collections.Generic
open Pulumi.FSharp.Azure.Common
open Pulumi.Azure.Storage
open Pulumi.FSharp
open Pulumi

type Replication =
    | LRS
    
type Tier =
    | Standard

module StorageAccountInternal =
    type StorageAccountArgsRecord = {
        Replication: Replication
        Tier: Tier
        HttpsOnly: bool
    }

open StorageAccountInternal

type StorageAccountBuilder () =
    inherit AzureResource()
    
    member __.Yield _ = (AzureResource.Zero, {
        Replication = LRS
        Tier = Standard
        HttpsOnly = true
    })

    member __.Run (cargs, args) =
        cargs.Region |>
        regionName |>
        input |>
        (fun l  -> AccountArgs(Location = l,
                               ResourceGroupName = (getName (cargs.Extras |> getResourceGroup)),
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
    member __.Replication((cargs, args : StorageAccountArgsRecord), replication) =
        cargs, { args with Replication = replication }
    
    [<CustomOperation("tier")>]
    member __.Tier((cargs, args : StorageAccountArgsRecord), tier) =
        cargs, { args with Tier = tier }
    
    [<CustomOperation("httpsOnly")>]
    member __.HttpsOnly((cargs, args : StorageAccountArgsRecord), httpsOnly) =
        cargs, { args with HttpsOnly = httpsOnly }

let storageAccount = StorageAccountBuilder()