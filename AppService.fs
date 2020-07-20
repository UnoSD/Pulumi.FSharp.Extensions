[<AutoOpen>]
module Pulumi.FSharp.Azure.Legacy.AppService

open Pulumi.Azure.AppService.Inputs
open Pulumi.FSharp.Azure.Regions
open Pulumi.FSharp.Azure.Common
open Pulumi.Azure.AppService
open Pulumi.FSharp

type Tier =
    | Dynamic
    
type Kind =
    | Windows
    | App
    | Linux
    | Elastic
    | FunctionAppKind
    
type Size =
    | Y1
    
module AppServiceInternal =    
    type AppServiceArgsRecord = {
        Tier: Tier
        Kind: Kind
        Size: Size
    }
    
let private getKind =
    function
    | Windows
    | App             -> "Windows"
    | Linux           -> "Linux"
    | Elastic         -> "elastic"
    | FunctionAppKind -> "FunctionApp"
    
let private getTier =
    function
    | Dynamic -> "Dynamic"

open AppServiceInternal

type AppServiceBuilder internal () =
    inherit AzureResource()
    
    member __.Yield _ = (AzureResource.Zero, {
        Tier = Dynamic
        Kind = Windows
        Size = Y1
    })

    member __.Run (cargs, args) =
        PlanSkuArgs(Tier = (input (getTier args.Tier)),
                   Size = input (match args.Size with | Y1 -> "Y1"))
        |> (fun psa -> PlanArgs(ResourceGroupName = (getName (cargs.Extras |> getResourceGroup)),
                            Location = input (regionName cargs.Region),
                            Kind = input (getKind args.Kind),
                            Sku = input psa))
        |> fun pa -> Plan(cargs.Name, pa)
    
    [<CustomOperation("tier")>]
    member __.Tier((cargs, args), tier) = cargs, { args with Tier = tier }        
    
    [<CustomOperation("size")>]
    member __.Size((cargs, args), size) = cargs, { args with Size = size }
    
    [<CustomOperation("kind")>]
    member __.Kind((cargs, args), kind) = cargs, { args with Kind = kind }

let appService = AppServiceBuilder()