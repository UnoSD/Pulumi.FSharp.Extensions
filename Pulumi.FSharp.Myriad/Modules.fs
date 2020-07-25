module AstModules

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
    
    let allNestedTypes =
        [
            for (_, jsonValue) in schema.["resources"].Properties() do
                for (property, jsonValue) in jsonValue.Properties() do
                    if property = "inputProperties" then
                        for (_, jsonValue) in jsonValue.Properties() do
                            for tuple in jsonValue.Properties() do
                                 match tuple with 
                                 | ("type", JsonValue.String("array")) -> match jsonValue.["items"]
                                                                                         .TryGetProperty("$ref") with
                                                                          | Some jsonValue -> yield jsonValue.AsString()
                                                                                                             .Substring(8)
                                                                          | _      -> ()
                                 | ("$ref", JsonValue.String(type'))   -> yield type'.Substring(8)
                                 | _                                   -> ()
        ]
    
    let pulumiProviderName =
        schema.["name"].AsString()
    
    let inline typedMatches (property : string) (regex : ^a) builderType filter =
        let getTypedMatch type' = (^a : (member TypedMatch : string -> 'b) (regex, type'))
        
        schema.[property].Properties() |>
        filter |>
        Array.map (fun (type', jsonValue) -> getTypedMatch type' |> builderType, jsonValue)
        
    let inline flip f x y =
        f y x
        
    let types =
        typedMatches "types" typeInfo Type <| Array.filter (fst >> (flip List.contains) allNestedTypes)
    
    let resources =
        typedMatches "resources" resourceInfo Resource id
    
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
            Array.filter (fst >> (function | Type x when // Fix this
                                                         x.ResourceType.Value = "AccountNetworkRules" -> false
                                           | _ -> true)) |>
            Array.Parallel.collect createBuilders
        
        let anyType =
            function
            | Type _ -> true
            | _      -> false
        
        let hasTypes =
            builders |>
            Array.exists (fst >> anyType)
        
        let openInputs =
            if hasTypes then
                [ Module.open'("Pulumi." + namespaces.[pulumiProviderName] + "." + moduleName + ".Inputs") ]
            else
                []
            
        createModule moduleName namespaces.[pulumiProviderName] openInputs types
    
    let invalidProvidersList =
        [ "config"; "index"; "" ]
    
    let doesNot =
        not
    
    let contain =
        List.contains
    
    Array.concat [ types; resources ] |>
    Array.groupBy resourceProvider |>
    debugFilterProvider |>
    Array.filter (fun (_, builders) -> not <| Array.isEmpty builders) |>
    Array.filter (fun (provider, _) -> invalidProvidersList |> (doesNot << contain provider)) |>
    Array.Parallel.map createProviderModule