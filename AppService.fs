namespace Pulumi.FSharp.Azure

open Pulumi.Azure.AppService.Inputs
open Pulumi.FSharp.Azure.Regions
open Pulumi.FSharp.Azure.Core
open Pulumi.Azure.AppService
open Pulumi.Azure.Core
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
        ResourceGroup: IOArg<ResourceGroup>
        Tier: Tier
        Kind: Kind
        Size: Size
    }

    // Should not default to FunctionApp it's an App Service after all
    // Maybe expose two builders and parameterize the default type in
    // the constructor
    type AppServiceBuilder internal (defaultKind) =
        inherit AzureResource()
        
        member __.Yield _ = (AzureResource.Zero, {
            ResourceGroup = Name ""
            Tier = Dynamic
            Kind = defaultKind
            Size = Y1
        })

        member __.Run (cargs, args) =
           PlanSkuArgs(Tier = (input <| getTier args.Tier),
                       Size = input (match args.Size with | Y1 -> "Y1")) |>
           (fun psa -> PlanArgs(ResourceGroupName = (getName args.ResourceGroup),
                                Location = input (regionName cargs.Region),
                                Kind = input (getKind args.Kind),
                                Sku = input psa)) |>           
           fun pa -> Plan(cargs.Name, pa)
        
        [<CustomOperation("tier")>]
        member __.Tier((cargs, args), tier) = cargs, { args with Tier = tier }
        
        [<CustomOperation("resourceGroup")>]
        member __.ResourceGroup((cargs, args), resourceGroup) = cargs, {
            args with ResourceGroup = Object resourceGroup
        }
        
        member __.ResourceGroup((cargs, args), resourceGroup) = cargs, {
            args with ResourceGroup = Name resourceGroup
        }

    let appService = AppServiceBuilder(Windows)
    let functionAppService = AppServiceBuilder(FunctionAppKind)