namespace Pulumi.FSharp.Azure

open Pulumi.FSharp.Azure.Core
open Pulumi.Azure.Core
open Pulumi.FSharp
open Pulumi

[<AutoOpen>]
module ResourceGroup =
    type ResourceGroupArg =
        | ResourceGroupObject of ResourceGroup
        | ResourceGroupName of string
    
    let getResourceGroupInput = 
        function
        | ResourceGroupObject rg -> io rg.Name
        | ResourceGroupName name -> input name
    
    type ResourceGroupBuilderArgs = {
        Name: string
        Region: Region
        Tags: (string * Input<string>) list
    }

    type ResourceGroupBuilder () =
        member __.Yield _ = {
            Name = "" // This needs to be an option or mandatory
            Region = WestEurope
            Tags = []
        }

        member __.Run args =
            args.Region |>
            regionName |>
            input |>
            (fun l  -> ResourceGroupArgs(Location = l, Tags = inputMap args.Tags)) |>
            fun rga -> ResourceGroup(args.Name, rga)

        [<CustomOperation("name")>]
        member __.Name(args, name) = { args with Name = name }

        [<CustomOperation("region")>]
        member __.Region(args, region) = { args with Region = region }
        
        [<CustomOperation("tags")>]
        member __.Tags(args, tags) = { args with Tags = tags }
        
        member __.Tags(args, tags) = { args with Tags = tags |>
                                                        List.map (fun (n, v) -> (n, input v)) }

    let resourceGroup = ResourceGroupBuilder()