namespace Pulumi.FSharp.Azure

open Pulumi.Azure.AppService.Inputs
open Pulumi.FSharp.Azure.Regions
open Pulumi.FSharp.Azure.Core
open Pulumi.Azure.AppService
open Pulumi.FSharp

[<AutoOpen>]
module AppService =        
    type Tier =
        | Dynamic
        
    type Kind =
        | Windows
        | App
        | Linux
        | Elastic
        | FunctionAppKind
        
    let getKind =
        function
        | Windows
        | App             -> "Windows"
        | Linux           -> "Linux"
        | Elastic         -> "elastic"
        | FunctionAppKind -> "FunctionApp"
        
    type Size =
        | Y1
        
    let getTier =
        function
        | Dynamic -> "Dynamic"

    type AppServiceArgsRecord = {
        Tier: Tier
        Kind: Kind
        Size: Size
    }

    type AppServiceBuilder () =
        inherit AzureResource()
        
        member __.Yield _ = (AzureResource.Zero, {
            Tier = Dynamic
            Kind = Windows
            Size = Y1
        })

        member __.Run (cargs, args) =
           PlanSkuArgs(Tier = (input <| getTier args.Tier),
                       Size = input (match args.Size with | Y1 -> "Y1")) |>
           (fun psa -> PlanArgs(ResourceGroupName = (getName (cargs.Extras |> getResourceGroup)),
                                Location = input (regionName cargs.Region),
                                Kind = input (getKind args.Kind),
                                Sku = input psa)) |>           
           fun pa -> Plan(cargs.Name, pa)
        
        [<CustomOperation("tier")>]
        member __.Tier((cargs, args), tier) = cargs, { args with Tier = tier }        
        
        [<CustomOperation("size")>]
        member __.Size((cargs, args), size) = cargs, { args with Size = size }
        
        [<CustomOperation("kind")>]
        member __.Kind((cargs, args), kind) = cargs, { args with Kind = kind }

    let appService = AppServiceBuilder()