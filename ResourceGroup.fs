[<AutoOpen>]
module Pulumi.FSharp.Azure.Legacy.ResourceGroup

open Pulumi.FSharp.Azure.Regions
open Pulumi.FSharp.Azure.Common
open Pulumi.Azure.Core
open Pulumi.FSharp

type ResourceGroupBuilder internal () =
    inherit AzureResource ()
    
    member __.Yield _ =
        (AzureResource.Zero, ())

    member __.Run (cargs, _) =
        cargs
        |> (fun args -> args.Region)
        |> regionName
        |> input
        |> (fun l  -> ResourceGroupArgs(Location = l, Tags = inputMap cargs.Tags))
        |> fun rga -> ResourceGroup(cargs.Name, rga)

let resourceGroup = ResourceGroupBuilder()