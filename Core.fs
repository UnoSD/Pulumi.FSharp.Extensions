module Pulumi.FSharp.Azure.Core

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
    
type AzureResourceArgs = {
    Name: string
    Region: Region // From resource group if None
    Tags: (string * Input<string>) list
}

type AzureResourceGroup () =
    //abstract member __Yield a
    //abstract member __Run a
    
    static member Zero = {
         Name = "" // This needs to be an option or mandatory
         Tags = []
         Region = WestEurope
     }
    
    [<CustomOperation("name")>]
    member __.Name((cra, rga), name) = { cra with Name = name }, rga
    
    [<CustomOperation("region")>]
    member __.Region((cra, rga), region) = cra, { rga with Region = region }

    [<CustomOperation("tags")>]
    member __.Tags((cra, rga), tags) = { cra with Tags = tags }, rga
    
    member __.Tags((cra, rga), tags) = { cra with Tags = tags |> List.map (fun (n, v) -> (n, input v)) }, rga