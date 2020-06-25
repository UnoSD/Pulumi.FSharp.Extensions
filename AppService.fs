namespace Pulumi.FSharp.Azure

open Pulumi.Azure.AppService.Inputs
open Pulumi.Azure.AppService
open Pulumi.FSharp.Azure.Core
open Pulumi.FSharp
open Pulumi

[<AutoOpen>]
module AppService =        
    type Tier =
        | Dynamic
        
    type Kind =
        | FunctionAppKind
        
    type Size =
        | Y1
        
    let getTier =
        function
        | Dynamic -> "Dynamic"

    type AppServiceArgsRecord = {
        Name: string
        Region: Region
        Tags: (string * Input<string>) list
        ResourceGroup: ResourceGroupArg
        Tier: Tier
        Kind: Kind
        Size: Size
    }

    // Should not default to FunctionApp it's an App Service after all
    // Maybe expose two builders and parameterize the default type in
    // the constructor
    type AppServiceBuilder internal () =
        member __.Yield _ = {
            Name = ""
            Region = WestEurope
            Tags = []
            ResourceGroup = ResourceGroupName ""
            Tier = Dynamic
            Kind = FunctionAppKind
            Size = Y1
        }

        member __.Run args =
           PlanSkuArgs(Tier = (input <| getTier args.Tier),
                       Size = input (match args.Size with | Y1 -> "Y1")) |>
           (fun psa -> PlanArgs(ResourceGroupName = (getResourceGroupInput args.ResourceGroup),
                                Location = input (regionName args.Region),
                                Kind = input (match args.Kind with | FunctionAppKind -> "FunctionApp"),
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
            args with ResourceGroup = ResourceGroupObject resourceGroup
        }
        
        member __.ResourceGroup(args : AppServiceArgsRecord, resourceGroup) = {
            args with ResourceGroup = ResourceGroupName resourceGroup
        }
        
        // Can a custom operation have two arguments? CommonArgs + AppService args?
        // If so, we can inherit this from a base class with generic member (args, cargs, tags)
        [<CustomOperation("tags")>]
        member __.Tags(args : AppServiceArgsRecord, tags) = { args with Tags = tags }

        member __.Tags(args : AppServiceArgsRecord, tags) = { args with Tags = tags |>
                                                                               List.map (fun (n, v) -> (n, input v)) }

    let appService = AppServiceBuilder()