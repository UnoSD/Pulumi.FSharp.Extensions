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
        
        createLet (toCamelCase (typeName))
                  (createInstance (typeName + "Builder") SynExpr.CreateUnit)
                  (seq { yield "*** Available properties ***"
                         yield ""
                         yield "When names are available on the resource,"
                         yield "**resourceName** maps to the name of the"
                         yield "provider resource, **name** maps to the"
                         yield "Pulumi name"
                         yield ""
                         yield! properties |> Array.map fst })
        
        // Create also shortcut lets:
        // let storageOsDisk = virtualMachineStorageOsDisk        
    |]

let private getSchemaFromCacheOrUrl schemaUrl providerName version =
    let fileName = providerName + "." + version + ".json"
    
    if File.Exists(fileName) then
        File.ReadAllText(fileName)
    else
        let json =
            Http.RequestString(schemaUrl)
        
        #if DEBUG
        File.WriteAllText(fileName, json)
        #endif

        json
    
let private createModule name namespace' types =
    Module.module'(name, [
        yield  Module.open'("Pulumi." + namespace' + "." + name)

        yield! types
    ])

let createPulumiModules schemaUrl providerName version =
    let schema =
        getSchemaFromCacheOrUrl schemaUrl providerName version |>
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
        typedMatches "resources" resourceInfo Resource <|
        Array.filter (fun (_, v) -> v.TryGetProperty("deprecationMessage").IsNone)
    
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
    
    let invalidProvidersList =
        [ "config"; "index"; "" ]
    
    let doesNot =
        not
    
    let contain =
        List.contains
    
    let filters =
        debugFilterProvider >>
        Array.filter (fun (_, builders) -> not <| Array.isEmpty builders) >>
        Array.filter (fun (provider, _) -> invalidProvidersList |> (doesNot << contain provider))
    
    let createBuildersParallelFiltered typesOrResources =
        Array.groupBy resourceProvider typesOrResources |>
        filters |>
        Map.ofArray |>
        Map.map (fun _ typesOrResources -> typesOrResources |>
                                           debugFilterTypes |>
                                           Array.Parallel.collect createBuilders)
        
    let typeBuilders =
        createBuildersParallelFiltered types
        
    let resourceBuilders =
        createBuildersParallelFiltered resources
    
    Map.fold (fun acc provider resourceBuilders ->
                    let typesModule =
                        typeBuilders |>
                        Map.tryFind provider |>
                        Option.bind (fun providerTypeBuilders -> if Array.isEmpty providerTypeBuilders then None else Some providerTypeBuilders) |>
                        Option.map (fun providerTypeBuilders -> [|createModule "Inputs" (namespaces.[pulumiProviderName] + "." + namespaces.[provider]) providerTypeBuilders|]) |>
                        Option.defaultValue [||]
                    
                    let moduleContent =
                        Array.append typesModule resourceBuilders
                    
                    (createModule namespaces.[provider] namespaces.[pulumiProviderName] moduleContent) :: acc)
             []
             resourceBuilders