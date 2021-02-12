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

let private createModuleContent allTypes (properties : (string * JsonValue) []) typeName isType =
    [|
        createBuilderClass allTypes isType typeName properties
        
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
    
let rec private createModule (name : string) (namespace' : string) types =
    match name.Split('.') with
    | [| name |] -> Module.module'(name, [
                        if name = "Inputs" && namespace'.Contains("Kubernetes") then
                            ()
                        else
                            yield  Module.open'("Pulumi." + namespace' + "." + name)

                        yield! types
                    ])
    | [| name; subname |] -> Module.module'(name, [
                                Module.open'("Pulumi." + namespace' + ".Types.Inputs." + name + "." + subname)
                                
                                createModule subname (namespace' + "." + name) types
                            ])
    | _ -> failwith "Too many dots"
    
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
        
    let resources =
        typedMatches "resources" resourceInfo Resource <|
        Array.filter (fun (_, v) -> v.TryGetProperty("deprecationMessage").IsNone)
        
    let types =
        typedMatches "types" typeInfo Type <|
        Array.filter (fst >> (flip List.contains) allNestedTypes)
    
    let resourceProvider (builder, _) =
        match builder with
        | Type t     -> if t.SubNamespace.Value = t.ResourceType.Value then
                            t.ResourceProviderNamespace.Value
                        else
                            t.ResourceProviderNamespace.Value + "/" + t.SubNamespace.Value
        | Resource r -> if (r.SubNamespace.Value |> toPascalCase) = r.ResourceType.Value then
                            r.ResourceProviderNamespace.Value
                        else
                            r.ResourceProviderNamespace.Value + "/" + r.SubNamespace.Value
    
    let namespaces =
        schema.["language"]
              .["csharp"]
              .["namespaces"]
              .Properties() |>
        Array.map (fun (p, jv) -> (p, jv.AsString())) |>
        Map.ofArray
    
    let create allTypes (jsonValue : JsonValue) (propertyName : string) typeName isType =
        let properties =
            match jsonValue.TryGetProperty(propertyName) with
            | Some value -> value.Properties()
            | None       -> [||]
            
        createModuleContent allTypes properties typeName isType
    
    let createBuilders allTypes (typeInfo, (jsonValue : JsonValue)) =
        match typeInfo with
        | Type t     -> create allTypes jsonValue "properties" t.ResourceType.Value true
        | Resource r -> create allTypes jsonValue "inputProperties" r.ResourceType.Value false
    
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
    
    let createBuildersParallelFiltered allTypes typesOrResources =
        Array.groupBy resourceProvider typesOrResources |>
        filters |>
        Map.ofArray |>
        Map.map (fun _ typesOrResources -> typesOrResources |>
                                           debugFilterTypes |>
                                           Array.Parallel.collect (createBuilders allTypes))
        
    let allAvailableTypes =
        schema.["types"].Properties() |> Array.map fst
        
    let typeBuilders =
        createBuildersParallelFiltered allAvailableTypes types
        
    let resourceBuilders =
        createBuildersParallelFiltered allAvailableTypes resources
    
    let providerNamespace =
        match namespaces.TryGetValue(pulumiProviderName) with
        | (true, value) -> value
        | _             -> pulumiProviderName |> toPascalCase
    
    Map.fold (fun acc provider resourceBuilders ->
                    let resourceProviderNamespace =
                        namespaces.[provider]
                    
                    let typesModule =
                        typeBuilders |>
                        Map.tryFind provider |>
                        Option.bind (fun providerTypeBuilders -> if Array.isEmpty providerTypeBuilders then None else Some providerTypeBuilders) |>
                        Option.map (fun providerTypeBuilders -> [|createModule "Inputs" (providerNamespace + "." + resourceProviderNamespace) providerTypeBuilders|]) |>
                        Option.defaultValue [||]
                    
                    let moduleContent =
                        Array.append typesModule resourceBuilders
                    
                    (createModule resourceProviderNamespace providerNamespace moduleContent) :: acc)
             []
             resourceBuilders