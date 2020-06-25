namespace Pulumi.FSharp.Azure

open Pulumi.Azure.AppService.Inputs
open Pulumi.FSharp.Azure.Regions
open Pulumi.FSharp.Azure.Core
open Pulumi.Azure.AppService
open Pulumi.Azure.Core
open Pulumi.FSharp
open Pulumi

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
        Name: string
        Region: Region
        Tags: (string * Input<string>) list
        ResourceGroup: IOArg<ResourceGroup>
        Tier: Tier
        Kind: Kind
        Size: Size
    }

    // Should not default to FunctionApp it's an App Service after all
    // Maybe expose two builders and parameterize the default type in
    // the constructor
    type AppServiceBuilder internal (defaultKind) =
        member __.Yield _ = {
            Name = ""
            Region = WestEurope
            Tags = []
            ResourceGroup = Name ""
            Tier = Dynamic
            Kind = defaultKind
            Size = Y1
        }

        member __.Run args =
           PlanSkuArgs(Tier = (input <| getTier args.Tier),
                       Size = input (match args.Size with | Y1 -> "Y1")) |>
           (fun psa -> PlanArgs(ResourceGroupName = (getName args.ResourceGroup),
                                Location = input (regionName args.Region),
                                Kind = input (getKind args.Kind),
                                Sku = input psa)) |>           
           fun pa -> Plan(args.Name, pa)

        [<CustomOperation("name")>]
        member __.Name(args : AppServiceArgsRecord, name) = { args with Name = name }

        [<CustomOperation("region")>]
        member __.Region(args : AppServiceArgsRecord, region) = { args with Region = region }
        
        [<CustomOperation("tier")>]
        member __.Tier(args : AppServiceArgsRecord, tier) = { args with Tier = tier }
        
        [<CustomOperation("resourceGroup")>]
        member __.ResourceGroup(args : AppServiceArgsRecord, resourceGroup) = {
            args with ResourceGroup = Object resourceGroup
        }
        
        member __.ResourceGroup(args : AppServiceArgsRecord, resourceGroup) = {
            args with ResourceGroup = Name resourceGroup
        }
        
        // Can a custom operation have two arguments? CommonArgs + AppService args?
        // If so, we can inherit this from a base class with generic member (args, cargs, tags)
        [<CustomOperation("tags")>]
        member __.Tags(args : AppServiceArgsRecord, tags) = { args with Tags = tags }

        member __.Tags(args : AppServiceArgsRecord, tags) = { args with Tags = tags |>
                                                                               List.map (fun (n, v) -> (n, input v)) }

    let appService = AppServiceBuilder(Windows)
    let functionAppService = AppServiceBuilder(FunctionAppKind)