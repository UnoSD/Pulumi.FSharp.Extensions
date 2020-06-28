[<AutoOpen>]
module Pulumi.FSharp.Azure.ResourceGroup

open Pulumi.FSharp.Azure.Regions
open Pulumi.FSharp.Azure.Core
open Pulumi.Azure.Core
open Pulumi.FSharp

type ResourceGroupBuilder () =
    inherit AzureResource ()
    
    member __.Yield _ =
        (AzureResource.Zero, None)

    member __.Run (cargs, _) =
        cargs |>
        (fun args -> args.Region) |>
        regionName |>
        input |>
        (fun l  -> ResourceGroupArgs(Location = l, Tags = inputMap cargs.Tags)) |>
        fun rga -> ResourceGroup(cargs.Name, rga)

let resourceGroup = ResourceGroupBuilder()