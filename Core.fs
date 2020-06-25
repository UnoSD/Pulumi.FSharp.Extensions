namespace Pulumi.FSharp.Azure

open Microsoft.FSharp.Reflection
open Pulumi.FSharp
open Pulumi

module Core =
    type IOArg<'a> =
        | Object of 'a
        | Name of string
        | IO of Output<string>
    
    let inline getName ioArg = 
        match ioArg with
        | Object o -> (^a : (member Name: Output<string>) (o)) |> io
        | Name n -> input n
        | IO i -> io i
        
    let getUnionCaseName<'a> case = 
        match FSharpValue.GetUnionFields(case, typeof<'a>) with
        | case, _ -> case.Name  