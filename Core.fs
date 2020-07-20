module Pulumi.FSharp.Azure.Common

open Pulumi.Azure.Core
open Pulumi.FSharp.Azure.Regions
open Microsoft.FSharp.Reflection
open Pulumi.FSharp
open Pulumi

type IOArg<'a> =
    | Object of 'a
    | Name of string
    | IO of Output<string>

let inline getName ioArg = 
    match ioArg with
    | Object o -> (^a : (member Name: Output<string>) (o)) |> io
    | Name n -> input n
    | IO i -> io i
    
let getUnionCaseName (case : 'a) = 
    match FSharpValue.GetUnionFields(case, typeof<'a>) with
    | case, _ -> case.Name
    
type AzureResourceExtraArgs =
    | ResourceGroupArg of IOArg<ResourceGroup>
    
type AzureResourceArgs = {
    Name: string
    Region: Region // From resource group if None
    Tags: (string * Input<string>) list
    Extras: AzureResourceExtraArgs list
}

let getResourceGroup =
    List.tryPick (fun x -> match x with | ResourceGroupArg x -> Some x) >>
    Option.defaultValue (Name "")

[<AbstractClass>]
type AzureResource internal () =
    let addOrReplaceResourceGroup extras resourceGroup =
        (resourceGroup |> ResourceGroupArg) ::
        (
            extras
            |>List.filter (fun x -> match x with | ResourceGroupArg _ -> false))

    let rg cargs resourceGroup =
        Object resourceGroup
        |> addOrReplaceResourceGroup cargs.Extras
            
    //abstract member __Yield : 'a -> AzureResourceArgs * 'b
    //abstract member __Run : AzureResourceArgs * 'b -> 'a
        
    static member Zero = {
         Name = "" // This needs to be an option or mandatory
         Tags = []
         Region = WestEurope
         Extras = [ Name "" |> ResourceGroupArg ]
     }
    
    [<CustomOperation("name")>]
    member __.Name((cra, rga), name) =
        { cra with Name = name }, rga
    
    [<CustomOperation("region")>]
    member __.Region((cra, rga), region) =
        cra, { rga with Region = region }

    [<CustomOperation("tags")>]
    member __.Tags((cra, rga), tags) =
        { cra with Tags = tags }, rga
    
    member __.Tags((cra, rga), tags) =
        { cra with Tags = tags |> List.map (fun (n, v) -> (n, input v)) }, rga

    [<CustomOperation("resourceGroup")>]
    member __.ResourceGroup((cargs, args), resourceGroup) =
        { cargs with Extras = (rg cargs resourceGroup) }, args
    
    member __.ResourceGroup((cargs, args), resourceGroup) =
        { cargs with Extras = (addOrReplaceResourceGroup cargs.Extras (Name resourceGroup)) }, args
    
    member __.ResourceGroup((cargs, args), resourceGroup) =
        { cargs with Extras = (addOrReplaceResourceGroup cargs.Extras (IO resourceGroup)) }, args