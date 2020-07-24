module AstModules

open System
open FSharp.Compiler.SyntaxTree
open AstInstance
open FSharp.Data
open AstHelpers
open AstBuilder
open System.IO
open AstLet
open Debug
open FsAst
open Core

let private createModuleContent (properties : (string * JsonValue) []) typeName isType =
    [|
        createBuilderClass isType typeName properties
        
        createLet (toCamelCase (typeName)) (createInstance (typeName + "Builder") SynExpr.CreateUnit)     
        
        // Create also shortcut lets:
        // let storageOsDisk = virtualMachineStorageOsDisk        
    |]

let private getSchemaFromCacheOrUrl schemaUrl providerName =
    let fileName = providerName + ".json"
    
    if File.Exists(fileName) then
        File.ReadAllText(fileName)
    else
        let json =
            Http.RequestString(schemaUrl)
        
        #if DEBUG
        File.WriteAllText(fileName, json)
        #endif

        json
    
let private createModule name namespace' openInputs types =
    Module.module'(name, [
        yield  Module.open'("Pulumi." + namespace' + "." + name)
        yield! openInputs

        yield! types
    ])
    
let createPulumiModules schemaUrl providerName =
    let schema =
        getSchemaFromCacheOrUrl schemaUrl providerName |>
        JsonValue.Parse
    
    let pulumiProviderName =
        schema.["name"].AsString()
    
    let typeMatches =
        schema.["types"].Properties() |>
        Array.map (fun (type', jv) -> typeInfo.TypedMatch(type'), jv)
    
    let types =
        typeMatches |>
        Array.map (fun (typedMatch, jsonValue) -> (Type typedMatch, jsonValue))
    
    let resources =
        schema.["resources"].Properties() |>
        Array.map (fun (type', jv) -> resourceInfo.TypedMatch(type') |> Resource, jv)
    
    let resourceProvider (builder, _) =
        match builder with
        | Type t     -> t.ResourceProviderNamespace.Value
        | Resource r -> r.ResourceProviderNamespace.Value
    
    let namespaces =
        schema.["language"]
              .["csharp"]
              .["namespaces"]
              .Properties() |>
        Array.map (fun (p, jv) -> (p, jv.AsString())) |>
        Map.ofArray
    
    let create (jsonValue : JsonValue) (propertyName : string) typeName isType =
        let properties =
            match jsonValue.TryGetProperty(propertyName) with
            | Some value -> value.Properties()
            | None       -> [||]
            
        createModuleContent properties typeName isType
    
    let createBuilders (typeInfo, (jsonValue : JsonValue)) =
        match typeInfo with
        | Type t     -> create jsonValue "properties" t.ResourceType.Value true
        | Resource r -> create jsonValue "inputProperties" r.ResourceTypePascalCase.Value false
    
    let createProviderModule (providerName, builders) =
        let moduleName =
            match namespaces |> Map.tryFind providerName with
            | Some ns -> ns
            | None    -> sprintf "Unable to find namespace %s" providerName |> failwith
        
        let types =
            builders |>
            debugFilterTypes |>
            Array.filter (fst >> (function | Type x when x.ResourceType.Value.StartsWith("get", StringComparison.Ordinal) ||
                                                         // Fix this
                                                         x.ResourceType.Value = "AccountNetworkRules" -> false
                                           | _ -> true)) |>
            Array.Parallel.collect createBuilders
        
        let isGet =
            function
            | Type t when not <| t.ResourceType.Value.StartsWith("get", StringComparison.Ordinal) -> true
            | _                                                                                   -> false
        
        let hasTypes =
            builders |>
            Array.exists (fst >> isGet)
        
        let openInputs =
            if hasTypes then
                [ Module.open'("Pulumi." + namespaces.[pulumiProviderName] + "." + moduleName + ".Inputs") ]
            else
                []
            
        createModule moduleName namespaces.[pulumiProviderName] openInputs types
    
    Array.concat [ types; resources ] |>
    Array.groupBy resourceProvider |>
    debugFilterProvider |>
    Array.filter (fun (_, builders) -> not <| Array.isEmpty builders) |>
    Array.filter (fun (provider, _) -> not <| List.contains provider [ "config"; "index"; "" ]) |>
    Array.Parallel.map createProviderModule