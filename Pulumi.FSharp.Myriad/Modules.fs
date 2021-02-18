module AstModules

open AstOperations
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
    
let createModule' name openNamespaces types =
    Module.module'(name, [
        yield! openNamespaces |> List.map (fun openNamespace -> Module.open'("Pulumi." + openNamespace))
                             
        yield! types
    ])
    
type PulumiModule = {
    CloudProviderNamespace: string
    ResourceProviderNamespace: string option
    Content: SynModuleDecl[]
}

let private nameAndType allTypes name (properties : (string * JsonValue) []) =
    let (|StartsWith|_|) (value : string) (text : string) =
        match text.StartsWith(value) with
        | true  -> String.length value |> text.Substring |> Some
        | false -> None

    let (|Property|_|) value seq =
        seq |> Seq.tryFind (fst >> ((=)value)) |> Option.map snd
    
    let typeMap =
        [ "string" , PString
          "number" , PFloat
          "integer", PInteger
          "boolean", PBoolean
          "Asset",   PAssetOrArchive
          "Any",     PAny
          "Archive", PArchive ] |> Map.ofList
    
    let typeExists typeName =
        Array.contains typeName allTypes
    
    let rec getTypeInfo : ((string * JsonValue) []) -> PType =
        function
        | Property("type") (JsonValue.String("array")) &
          Property("items") (JsonValue.Record(itemType))
            -> getTypeInfo itemType |> PType.PArray
          
        | Property("type") (JsonValue.String("object")) &
          Property("additionalProperties") (JsonValue.Record(itemType))
            -> getTypeInfo itemType |> PType.PMap
          
        | Property("type") (JsonValue.String("object")) &
          Property("$ref") (JsonValue.String("pulumi.json#/Json"))
            -> PType.PJson
          
        | Property("oneOf") (JsonValue.Array([| JsonValue.Record(one); JsonValue.Record(two) |]))
            -> match (getTypeInfo one, getTypeInfo two) with
               | one, two when one = two                              -> one
               | (PRef refType, other)
               | (other, PRef refType) when not <| typeExists refType -> other
               | one, two                                             -> PType.PUnion (one, two)

        | Property("type") (JsonValue.String("string")) &
          Property("$ref") (JsonValue.String(StartsWith("#/types/") typeQualified)) when not <| typeExists typeQualified
            -> PType.PString
                      
        | Property("$ref") (JsonValue.String(StartsWith("#/types/") typeQualified))
            -> PType.PRef typeQualified
          
        | Property("type") (JsonValue.String(baseType))
        | Property("$ref") (JsonValue.String(StartsWith("pulumi.json#/") baseType)) when (Map.containsKey baseType typeMap)
            -> typeMap.[baseType]
          
        | _ -> failwith ""
            
    let (Property("description") (JsonValue.String(description)), _) |
        (_, description) =
        properties, ""
    
    let (Property("language") (JsonValue.Record((Property("csharp") (JsonValue.Record(Property("name") (JsonValue.String(name))))))), _) |
        (_, name) =
        properties, name |> toPascalCase
    
    let deprecation =
        match properties with
        | Property("deprecationMessage") (JsonValue.String(message)) -> Deprecated message
        | _                                                          -> Current
    
    {
        Type = properties |> getTypeInfo
        Description = description
        Name = name
        Deprecation = deprecation
        GenerateYield = true
    }

let private createPTypes allTypes properties =
    let nameAndTypes =
        properties |>
        Array.map (fun (x, y : JsonValue) -> nameAndType allTypes x (y.Properties()))
        
    let (propOfSameComplexType, otherProperties) =
        nameAndTypes |>
        Array.groupBy (fun pt -> pt.Type) |>
        Array.partition (function | (PRef _, props) -> Array.length props > 1 | _ -> false) |>
        (fun (l, r) -> (l |> Array.collect snd,
                        r |> Array.collect snd))
        
    let propOfSameComplexTypeIgnoreComplex =
        propOfSameComplexType |>
        Array.map (fun td -> { td with GenerateYield = false })
        
    let order =
        nameAndTypes |> Array.map (fun td -> td.Name)
        
    Array.append propOfSameComplexTypeIgnoreComplex otherProperties |>
    Array.sortBy (fun n -> order |> Array.findIndex ((=)n.Name))

let createTypes (schema : JsonValue) =
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
    
    let create allTypes (propertiesFromResource : JsonValue option) (jsonValue : JsonValue) (propertyName : string) typeName isType =
        let properties =
            match isType, propertiesFromResource, lazy(jsonValue.TryGetProperty(propertyName)) with
            | true,  Some rip, _             -> rip.Properties()
            | _   ,  _       , Lazy(Some ip) -> ip.Properties()
            | _   ,  _       , Lazy(None)    -> [||]
            
        let pTypes =
            createPTypes allTypes properties
            
        [|
            createBuilderClass isType typeName pTypes
            
            createBuilderInstance typeName pTypes
        |]
    
    let createBuilders allTypes (schema : JsonValue) (typeInfo, (jsonValue : JsonValue)) =
        match typeInfo with
        | Type t     -> let propertiesFromResource =
                            schema.["resources"].TryGetProperty(t.Value) |>
                            Option.bind (fun r -> r.TryGetProperty("inputProperties"))
                        create allTypes propertiesFromResource jsonValue "properties" t.ResourceType.Value true
        | Resource r -> create allTypes None jsonValue "inputProperties" r.ResourceType.Value false
    
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
    
    let createBuildersParallelFiltered allTypes typesOrResources schema =
        Array.groupBy resourceProvider typesOrResources |>
        filters |>
        Map.ofArray |>
        Map.map (fun _ typesOrResources -> typesOrResources |>
                                           debugFilterTypes |>
                                           Array.Parallel.collect (createBuilders allTypes schema))
        
    let allAvailableTypes =
        schema.["types"].Properties() |> Array.map fst
        
    let typeBuilders =
        createBuildersParallelFiltered allAvailableTypes types schema
        
    let resourceBuilders =
        createBuildersParallelFiltered allAvailableTypes resources schema
    
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
    Map.fold folder List.empty |>
    List.partition (function
                    | { ResourceProviderNamespace = None } -> true
                    | _                                    -> false)