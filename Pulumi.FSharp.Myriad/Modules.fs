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

let private (|StartsWith|_|) (value : string) (text : string) =
    match text.StartsWith(value) with
    | true  -> String.length value |> text.Substring |> Some
    | false -> None

let private (|Property|_|) value seq =
    seq |> Seq.tryFind (fst >> ((=)value)) |> Option.map snd

let private (|PTArray|_|) =
    function
    | Property("type") (JsonValue.String("array")) &
      Property("items") (JsonValue.Record(itemType))
        -> Some itemType
    | _ -> None

let private (|PTMap|_|) =
    function
    | Property("type") (JsonValue.String("object")) &
      Property("additionalProperties") (JsonValue.Record(itemType))
        -> Some itemType
    | _ -> None
    
let private (|PTJson|_|) =
    function
    | Property("type") (JsonValue.String("object")) &
      Property("$ref") (JsonValue.String("pulumi.json#/Json"))
    | Property("$ref") (JsonValue.String("pulumi.json#/Json"))
        -> Some ()
    | _ -> None
    
let private (|PTUnion|_|) =
    function
    | Property("oneOf") (JsonValue.Array([| JsonValue.Record(one); JsonValue.Record(two) |]))
        -> Some (one, two)
    | _ -> None
    
let private (|PTRef|_|) =
    function
    | Property("type") (JsonValue.String("string")) &
      Property("$ref") (JsonValue.String(StartsWith("#/types/") typeQualified))
    | Property("$ref") (JsonValue.String(StartsWith("#/types/") typeQualified))
        -> Some typeQualified
    | _ -> None
    
let private (|PTBase|_|) =
    function
    | PTJson
    | PTMap _
    | PTUnion _
    | PTArray _ -> None
    | Property("type") (JsonValue.String(baseType))
    | Property("$ref") (JsonValue.String(StartsWith("pulumi.json#/") baseType))
        -> Some baseType
    | _ -> None

let private nameAndType isType allTypes name (properties : (string * JsonValue) []) =
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
        | PTArray itemType                                         -> getTypeInfo itemType |> PType.PArray
        | PTMap   itemType                                         -> getTypeInfo itemType |> PType.PMap
        | PTJson                                                   -> PType.PJson
        | PTRef typeQualified when not <| typeExists typeQualified -> PType.PString
        | PTRef typeQualified                                      -> PType.PRef typeQualified
        | PTBase baseType when (Map.containsKey baseType typeMap)  -> typeMap.[baseType]
        | PTUnion (one, two)
            -> match (getTypeInfo one, getTypeInfo two) with
               | one, two when one = two                              -> one
               | (PRef refType, other)
               | (other, PRef refType) when not <| typeExists refType -> other
               | one, two                                             -> PType.PUnion (one, two)          
        | x -> failwith $"Missing type pattern for {x}"
            
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
    
    let snakeCaseName =
        if name = "Name" && (not isType) then
            "resourceName"
        else 
            toCamelCase name
    
    let customOperationName =
        match snakeCaseName with
        | "resourceGroupName" -> "resourceGroup"
        | "name" when not isType -> "resourceName"
        | x -> x
    
    {
        Type = properties |> getTypeInfo
        Description = description
        Name = name
        OperationName = customOperationName
        Deprecation = deprecation
        CanGenerateYield = true
        IsResource = not isType
    }

let private createPTypes isType allTypes properties =
    let nameAndTypes =
        properties |>
        Array.map (fun (x, y : JsonValue) -> nameAndType isType allTypes x (y.Properties()))
        
    let (propOfSameComplexType, otherProperties) =
        nameAndTypes |>
        Array.groupBy (fun pt -> pt.Type) |>
        Array.partition (function | (PRef _, props) -> Array.length props > 1 | _ -> false) |>
        (fun (l, r) -> (l |> Array.collect snd,
                        r |> Array.collect snd))
        
    let propOfSameComplexTypeIgnoreComplex =
        propOfSameComplexType |>
        Array.map (fun td -> { td with CanGenerateYield = false })
        
    let order =
        nameAndTypes |> Array.map (fun td -> td.Name)
        
    Array.append propOfSameComplexTypeIgnoreComplex otherProperties |>
    Array.sortBy (fun n -> order |> Array.findIndex ((=)n.Name))

let createTypes (schema : JsonValue) =
    let allAvailableTypes =
        schema.["types"].Properties() |> Array.map fst
    
    let allTypes =
        schema.["types"].Properties() |>
        Map.ofArray
   
    let getPropertiesValues =
        function
        | JsonValue.Record(Property("inputProperties") (JsonValue.Record(jv)))
        | JsonValue.Record(Property("properties")      (JsonValue.Record(jv))) -> jv |> Array.map (snd >> (fun x -> x.Properties()))
        | _                                                                    -> [||]
        
    //let optionApply func =
    //    // Option.fold (fun o2 v1 -> o2 |> Option.map (func v1) |> Option.orElse (Some v1)) o2 o1
    //    function
    //    | Some value1, Some value2 -> func value1 value2 |> Some
    //    | None       , Some value
    //    | Some value , None        -> value |> Some
    //    | None       , None        -> None
                
    //let tupleMap f (one, two) =
    //    (f one, f two)
        
    let (<*>) fOpt xOpt =
        match fOpt, xOpt with
        | Some f, Some x -> Some (f x)
        | None  , Some x -> Some x
        | _              -> None
        
    let (<!>) =
        Option.map
                
    //let lift2 f xOpt yOpt =
    //    f <!> xOpt <*> yOpt

    //let (<*) x y =
    //    lift2 List.append x y
    
    let rec getRefType =
        function
        | PTRef t when Map.containsKey t allTypes -> Some [t]
        
        | PTMap t
        | PTArray t                               -> getRefType t
                                                  
        | PTUnion (a, b) (*PTUnion typeTuple*)    -> //typeTuple |> tupleMap getRefType |> optionApply (@)
                                                     //typeTuple |> tupleMap getRefType |> lift2 (@)
                                                     //typeTuple |>
                                                     //tupleMap getRefType ||>
                                                     //(fun x y -> List.append <!> x <*> y)                                                     
                                                     List.append <!> (getRefType a) <*> (getRefType b)
                                                     
        | PTMap t                                 -> getRefType t
                                                  
        | PTBase _                                
        | PTJson                                  -> None
        | x                                       -> failwith $"Pattern not matched {x}"
        
    let rec getAllNestedTypes refTypes resourceOrType =
        getPropertiesValues resourceOrType |>
        Array.choose getRefType |>
        List.concat |>
        (function
         | [] -> refTypes
         | a  -> a |> List.collect (fun refType -> match List.exists ((=)refType) refTypes with
                                                   | true  -> refTypes
                                                   | false -> allTypes.[refType] |>
                                                              getAllNestedTypes (refType :: refTypes)))
        
    let allNestedTypes =
        schema.["resources"].Properties() |>
        Array.map (snd >> getAllNestedTypes []) |>
        List.concat        
    
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
            createPTypes isType allTypes properties
            
        // Enums will fall into this category E.G. "kubernetes:core/v1:ServiceSpecType"
        // Not required as it will use an operation: type Pulumi.Kubernetes.Input.Types.Core.V1.ServiceSpecType.NodePort
        if Array.isEmpty pTypes then
            [||]
        else            
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