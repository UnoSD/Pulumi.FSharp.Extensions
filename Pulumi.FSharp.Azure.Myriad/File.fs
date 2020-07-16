module AstFile

open FSharp.Data
open AstNamespace
open AstModule
open AstType
open Debug

[<Literal>]
let private version =
    "3.11.0"

// Version needs to match NuGet package
[<Literal>]
let private pulumiSchemaUrl =
    "https://raw.githubusercontent.com/pulumi/pulumi-azure/v" + version + "/provider/cmd/pulumi-resource-azure/schema.json"

type private PulumiProvider =
    JsonProvider<pulumiSchemaUrl>

let createFile () =
    let provider = PulumiProvider.GetSample()
    
    let moduleWithType =
        createModule
    
    let modules =
        provider.Resources.JsonValue.Properties() |>
        //debugFilters |>
        Array.map (createType provider >>
                   moduleWithType) |>
        List.ofArray
    
    createNamespace modules