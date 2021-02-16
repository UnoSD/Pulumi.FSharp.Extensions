module AstModules

open BuilderInstance
open FSharp.Compiler.SyntaxTree
open FSharp.Data
open AstHelpers
open AstBuilder
open Debug
open Core

let rec createModule name openNamespace types =
    match name |> Option.map (String.split '.') with
    | None            -> Module.module'(openNamespace, [
                            Module.open'("Pulumi." + openNamespace)
                 
                            yield! types
                        ])
    | Some [| name |] -> let openNamespaces =
                            match name, String.split '.' openNamespace |> List.ofArray with
                            | "Inputs", "Kubernetes" :: _    -> []
                            | name    , "Kubernetes" :: tail ->
                                let sub = tail |> String.concat "."
                                let mn types = Module.open'("Pulumi." + "Kubernetes" + types + sub + "." + name)
                                [ mn ".Types.Inputs."
                                  mn "." ]
                            | name, _                        -> [ Module.open'("Pulumi." + openNamespace + "." + name) ]
                            
                         Module.module'(name, [
                             yield! openNamespaces
                             
                             yield! types
                         ])
    | Some [| name; subname |] -> Module.module'(name, [
                                    createModule (Some subname) (openNamespace + "." + name) types
                                ])
    | _ -> failwith "Too many dots"
    
type PulumiModule = {
    CloudProviderNamespace: string
    ResourceProviderNamespace: string option
    Content: SynModuleDecl[]
}

let createModules (schema : JsonValue) =
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
        Map.ofArray |>
        Map.map (fun _ jv -> jv.AsString() |> Some) |>
        Map.add "index" None
    
    let create allTypes (jsonValue : JsonValue) (propertyName : string) typeName isType =
        let properties =
            match jsonValue.TryGetProperty(propertyName) with
            | Some value -> value.Properties()
            | None       -> [||]
            
        [|
            createBuilderClass allTypes isType typeName properties
            
            createBuilderInstance typeName properties
        |]
    
    let createBuilders allTypes (typeInfo, (jsonValue : JsonValue)) =
        match typeInfo with
        | Type t     -> create allTypes jsonValue "properties" t.ResourceType.Value true
        | Resource r -> create allTypes jsonValue "inputProperties" r.ResourceType.Value false
    
    let invalidProvidersList =
        [ "config"; "" ]
    
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
    
    let cloudProviderNamespace =
        match namespaces.TryGetValue(pulumiProviderName) with
        | (true, Some value) -> value
        | _                  -> pulumiProviderName |> toPascalCase
    
    let folder modules resourceProvider resourceBuilders =
        let resourceProviderNamespace =
            namespaces.[resourceProvider]
        
        let openNamespace =
            resourceProviderNamespace |>
            Option.map (fun rpn -> $"{cloudProviderNamespace}.{rpn}") |>
            Option.defaultValue cloudProviderNamespace
        
        let typesModule =
            typeBuilders |>
            Map.tryFind resourceProvider |>
            Option.bind (fun providerTypeBuilders -> if Array.isEmpty providerTypeBuilders then None else Some providerTypeBuilders) |>
            Option.map (fun providerTypeBuilders -> [|createModule (Some "Inputs") openNamespace providerTypeBuilders|]) |>
            Option.defaultValue [||]
        
        let moduleContent =
            Array.append typesModule resourceBuilders
        
        {
            CloudProviderNamespace = cloudProviderNamespace
            ResourceProviderNamespace = resourceProviderNamespace
            Content = moduleContent
        } :: modules
    
    resourceBuilders |>
    Map.fold folder List.empty