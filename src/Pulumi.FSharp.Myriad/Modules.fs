module AstModules

open System
open System.Text.RegularExpressions
open FSharp.Compiler.Syntax
open BuilderInstance
open AstOperations
open FSharp.Data
open AstHelpers
open AstBuilder
open Debug
open Core

let inline equalsCI (str1) (str2) = String.Equals(str1, str2, StringComparison.OrdinalIgnoreCase)

let rec createModule name openNamespace types =
    match name |> Option.map (String.split '.') with
    | None            -> Module.module'(openNamespace, [
                            Module.open'("Pulumi." + openNamespace)
                            yield! types
                        ])
    | Some [| name |] -> 
        let openNamespaces =
            match name, String.split '.' openNamespace |> List.ofArray with
            // Kubernetes is the only provider, which overrides empty namespace with "Provider"
            | "Inputs", [ "Kubernetes"; "Provider" ] -> 
                [ Module.open'("Pulumi.Kubernetes.Types.Inputs.Provider") ]
            | "Inputs", "Kubernetes" :: _            ->
                []
            | name    , "Kubernetes" :: tail         ->
                let sub = tail |> String.concat "."
                let mn types = Module.open'("Pulumi." + "Kubernetes" + types + sub + "." + name)
                [ 
                    mn ".Types.Inputs."
                    mn "." 
                ]
            | name, _                        -> [ Module.open'("Pulumi." + openNamespace + "." + name) ]
            
        Module.module'(name, [
            yield! openNamespaces
            yield! types
        ])
    | Some [| name; subname |] -> 
        Module.module'(name, [
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
    seq |> Seq.tryFind (fst >> (=)value) |> Option.map snd

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
    
let private (|PTUnionMany|_|) =
    function
    | Property("oneOf") (JsonValue.Array(records)) when records |> Array.length > 2
        -> records |>
           Array.map (function | JsonValue.Record(x) -> x | _ -> failwith "Malformed union") |>
           Some
    | _ -> None
    
let private (|PTRef|_|) =
    function
    | Property("type") (JsonValue.String("string")) & Property("$ref") (JsonValue.String(StartsWith("#/types/") typeQualified))
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

let private nameAndType allPropertyNames isType allTypes name (properties : (string * JsonValue) []) =
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
        
    let rec getTypeInfo : (string * JsonValue) [] -> PType =
        function
        | PTArray itemType                                         -> getTypeInfo itemType |> PType.PArray
        | PTMap   itemType                                         -> getTypeInfo itemType |> PType.PMap
        | PTJson                                                   -> PType.PJson
        | PTRef typeQualified when not <| typeExists typeQualified -> PType.PString
        | PTRef typeQualified                                      -> PType.PRef typeQualified
        | PTBase baseType when (Map.containsKey baseType typeMap)  -> typeMap[baseType]
        | PTUnion (one, two)
            -> match (getTypeInfo one, getTypeInfo two) with
               | one, two when one = two                              -> one
               | PRef refType, other
               | other, PRef refType when not <| typeExists refType -> other
               | one, two                                             -> PType.PUnion (one, two)
        | PTUnionMany _
            -> PType.PAny
        | x -> failwith $"Missing type pattern for {x}"
            
    let Property("description") (JsonValue.String(description)), _ |
        _, description =
        properties, ""
    
    let Property("language") (JsonValue.Record(Property("csharp") (JsonValue.Record(Property("name") (JsonValue.String(name)))))), _ |
        _, name =
        properties, name |> toPascalCase
    
    let deprecation =
        match properties with
        | Property("deprecationMessage") (JsonValue.String(message)) -> Deprecated message
        | _                                                          -> Current
    
    let customOperationName =
        match name |> toCamelCase with
        | "name"          when not isType                                             -> "resourceName"
        | "resourceName"  when not isType                                             -> "providerResourceName"
        | "resourceType"  when allPropertyNames |> Array.contains "type"              -> "providerResourceType"
        | "resourceGroup" when allPropertyNames |> Array.contains "resourceGroupName" -> "providerResourceGroup"
        | "resourceGroupName"                                                         -> "resourceGroup"
        | "type"                                                                      -> "resourceType"
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
    let allPropertyNames =
        properties |> Array.map fst
    
    let nameAndTypes =
        properties |>
        Array.map (fun (x, y : JsonValue) -> nameAndType allPropertyNames isType allTypes x (y.Properties()))
        
    let propOfSameComplexType, otherProperties =
        nameAndTypes |>
        Array.groupBy (fun pt -> pt.Type) |>
        Array.partition (function | PRef _, props -> Array.length props > 1 | _ -> false) |>
        (fun (l, r) -> (l |> Array.collect snd,
                        r |> Array.collect snd))
        
    let propOfSameComplexTypeIgnoreComplex =
        propOfSameComplexType |>
        Array.map (fun td -> { td with CanGenerateYield = false })
        
    let order =
        nameAndTypes |> Array.map (fun td -> td.Name)
        
    Array.append propOfSameComplexTypeIgnoreComplex otherProperties |>
    Array.sortBy (fun n -> order |> Array.findIndex ((=)n.Name))

let private missingStatusTypes =
    [|
        "APIService"
        "CertificateSigningRequest"
        "CronJob"
        "CustomResourceDefinition"
        "DaemonSet"
        "Deployment"
        "FlowSchema"
        "HorizontalPodAutoscaler"
        "Ingress"
        "Job"
        "Namespace"
        "NetworkPolicy"
        "Node"
        "PersistentVolume"
        "PersistentVolumeClaim"
        "PersistentVolumeClaimPatch"
        "Pod"
        "PodDisruptionBudget"
        "PodScheduling"
        "PodSchedulingContext"
        "PriorityLevelConfiguration"
        "ReplicaSet"
        "ReplicationController"
        "ResourceClaim"
        "ResourceQuota"
        "Service"
        "ServiceCIDR"
        "StatefulSet"
        "StorageVersionMigration"
        "ValidatingAdmissionPolicy"
        "VolumeAttachment"
    |] |> Set.ofArray

let createTypes (schema : JsonValue) =

    let pulumiProviderName =
        schema["name"].AsString()
        
    let typesJson =
        match schema.TryGetProperty("types") with
        | Some types -> types.Properties()
        | None       -> [||]
    
    let allTypes =
        typesJson |> Map.ofArray

    let allAvailableTypes =
        typesJson |> Array.map fst

    let resourcesJson =
        schema["resources"].Properties() |>
        // Faking Provider to be in the root namespace
        Array.append [| $"{pulumiProviderName}:index:Provider", schema["provider"] |]

    let resourceTypes = Map.ofArray resourcesJson
    let allResources =
        resourcesJson |> Array.map fst

    let getPropertiesValues =
        function
        | JsonValue.Record(Property("inputProperties") (JsonValue.Record(jv)))
        | JsonValue.Record(Property("properties")      (JsonValue.Record(jv))) -> jv |> Array.map (snd >> (fun x -> x.Properties()))
        | _                                                                    -> [||]
        
    let (<*>) fOpt xOpt =
        match fOpt, xOpt with
        | Some f, Some x -> Some (f x)
        | None  , Some x -> Some x
        | _              -> None
        
    let (<!>) =
        Option.map
    
    let rec getRefType =
        function
        | PTRef t when Array.exists (equalsCI t) allAvailableTypes -> Some [t]
        | PTRef t when Array.exists (equalsCI t) allResources -> Some [t]
        | PTMap t
        | PTArray t -> getRefType t
        | PTUnion (a, b) ->
            List.append <!> (getRefType a) <*> (getRefType b)
        | PTMap t  -> getRefType t
        | PTUnionMany _                                                  
        | PTBase _                                
        | PTJson -> None
        | x                                       -> failwith $"Pattern not matched {x}"
        
    let rec getAllNestedTypes refTypes resourceOrType =

        let (|ExistsInCI|_|) collection input =
            if Seq.exists (equalsCI input) collection then Some () else None

        let (|GetCI|_|) (map: Map<_, JsonValue>) (key: string) =
            Map.tryPick (fun k v -> if equalsCI k key then Some v else None) map

        getPropertiesValues resourceOrType |>
        Array.choose getRefType |>
        List.concat 
        |> (function
            | [] -> refTypes
            | a  -> a |> List.collect (fun refType ->
                match refType with
                | ExistsInCI refTypes -> refTypes
                | GetCI allTypes typeJson
                | GetCI resourceTypes typeJson -> typeJson |> getAllNestedTypes (refType :: refTypes)
                | _ -> failwith "Referenced type does not exist in the Pulumi schema resources or types"
                )
        )

    let allNestedTypes =
        resourcesJson |>
        Array.map (snd >> getAllNestedTypes []) |>
        List.concat        
    
    let inline typedMatches jsonsArray (regex : ^a) builderType filter =
        let getTypedMatch type' = (^a : (member TypedMatch : string -> 'b) (regex, type'))
        
        jsonsArray |>
        filter |>
        Array.map (fun (type', jsonValue) -> getTypedMatch type' |> builderType, jsonValue)
        
    let inline flip f x y =
        f y x
        
    let resources =
        typedMatches resourcesJson typeInfoProvider Resource <|
        Array.filter (fun (k, v) -> v.TryGetProperty("deprecationMessage").IsNone && not (k.Contains("/get")))
        
    let types =
        typedMatches typesJson typeInfoProvider Type <|
        Array.filter (fst >> (flip List.contains) allNestedTypes)
    
    let tryGetMatch (regexMatch : Group) =
        if regexMatch.Success then
            Some regexMatch.Value
        else    
            None
    
    let getProvider builder =
        let t, subNamespaceOrName = 
            match builder with
            | Type     t -> t, tryGetMatch t.SubNamespace
            | Resource t -> t, tryGetMatch t.SubNamespace |> Option.map toPascalCase
        
        let namespace' =
            t.ResourceProviderNamespace.Value

        // Docker schema has inconsistency of case in resource type and name for some resources
        let resourceTypeAndNameComparison =
            match pulumiProviderName with
            | "docker" | "aws" -> StringComparison.OrdinalIgnoreCase
            | _ ->        StringComparison.Ordinal

        subNamespaceOrName 
        |> Option.bind (function
            | name when name.Equals(t.ResourceType.Value, resourceTypeAndNameComparison) -> None
            | _ -> namespace' + "/" + t.SubNamespace.Value |> Some) 
        |> Option.defaultValue namespace'
    
    let namespacesJson =
        match schema["language"].["csharp"].TryGetProperty("namespaces") with
        | Some ns -> ns.Properties()
        | None    -> [||]
    
    let namespaces =
        namespacesJson |>
        Map.ofArray |>
        Map.map (fun _ jv -> jv.AsString() |> Some) |>
        Map.add "index" None |>
        // Namespaces missing in the Pulumi.Command schema
        Map.add "local" (Some("Local")) |>
        Map.add "remote" (Some("Remote"))
    
    let create allTypes (jsonValue : JsonValue) (propertyName : string) typeName isType =
        
        let isComponent = if not isType then jsonValue.TryGetProperty "isComponent" |> Option.isSome else false

        let properties =
            match jsonValue.TryGetProperty(propertyName) with
            | Some ip -> ip.Properties()
            | None    -> [||]
        
        // Schema does not seem to match the library only for this particular
        // property and only in the Kubernetes provider. I hate to add exceptions
        // but I can't figure our why this happens; will ask the Pulumi team
        let properties =
            match pulumiProviderName, isType, typeName with
            | "kubernetes", true, x when missingStatusTypes |> Set.contains x
                -> properties |> Array.filter (function pName, _ -> pName <> "status")
            | "azure-native", false, x when Array.contains x [| "SecurityConnectorApplication"; "Application" |]
                -> properties |> Array.filter (function pName, _ -> pName <> "conditionSets")
            | "azure-native", false, x when Array.contains x [| "HybridRunbookWorkerGroup" |]
                -> properties |> Array.filter (function pName, _ -> pName <> "name")
            | _
                -> properties
        
        let description =
            match jsonValue.Properties() with
            | Property("description") (JsonValue.String(d)) -> d
            | _                                             -> "No description available"
            
        let pTypes =
            createPTypes isType allTypes properties
            
        let typeName =
            typeName |> toPascalCase
            
        // Enums will fall into this category E.G. "kubernetes:core/v1:ServiceSpecType"
        // Not required as it will use an operation: type Pulumi.Kubernetes.Input.Types.Core.V1.ServiceSpecType.NodePort
        if Array.isEmpty pTypes then
            [||]
        else            
            [|
                createBuilderClass isType isComponent typeName pTypes
                
                createBuilderInstance description typeName pTypes
            |]
    
    let createBuilders allTypes (typeInfo, jsonValue : JsonValue) =
        let typeName, isType, propertiesPropertyName =
            match typeInfo with
            | Type t     -> t.ResourceType.Value, true, "properties"
            | Resource t -> t.ResourceType.Value, false, "inputProperties"
        
        create allTypes jsonValue propertiesPropertyName typeName isType
    
    let invalidProvidersList =
        [ "config"; ""; "kustomize"; "apiextensions.k8s.io"; "yaml"; "helm.sh/v2" ]
    
    let doesNot =
        not
    
    let contain =
        List.contains
    
    let filters =
        debugFilterProvider >>
        Array.filter (fun (_, builders) -> not <| Array.isEmpty builders) >>
        Array.filter (fun (provider, _) -> invalidProvidersList |> (doesNot << contain provider))
    
    let filterKubernetesProblematicTypes types =
        types |>
        Array.filter (fun (bt, _) -> 
            match bt with
            | Type t -> not (t.ResourceType.Value = "FetchOpts" && t.ResourceProviderNamespace.Value = "helm.sh") 
            | Resource r -> not (r.ResourceType.Value = "Chart" && r.ResourceProviderNamespace.Value = "helm.sh"))
    
    let filterAzureNativeProblematicTypes types =
        types |>
        Array.filter (fun (bt, _) -> 
            match bt with
            | Type t -> not (t.ResourceType.Value = "ApplicationCondition" && t.ResourceProviderNamespace.Value = "security") 
            | _          -> true)
    
    let filterAwsProblematicTypes types =
        types |>
        Array.filter (fun (bt, _) -> 
            match bt with
            // this seems to be a nodejs-only mixin? CallbackFunctionArgs does not exist in the pulumi .net sdk
            | Resource r -> 
                not (r.ResourceType.Value = "CallbackFunction" && r.ResourceProviderNamespace.Value = "lambda")
            | Type t -> 
                not (t.ResourceType.Value = "CodePathOptions" && t.ResourceProviderNamespace.Value = "lambda"))

    let createBuildersParallelFiltered allTypes typesOrResources =
        Array.groupBy (fst >> getProvider) typesOrResources |>
        filters |>
        Map.ofArray |>
        Map.map (fun _ typesOrResources -> 
            typesOrResources |>
            debugFilterTypes |>
            filterKubernetesProblematicTypes |>
            filterAzureNativeProblematicTypes |>
            filterAwsProblematicTypes |>
            Array.Parallel.collect (createBuilders allTypes))
        
    let typeBuilders =
        createBuildersParallelFiltered allAvailableTypes types
        
    let resourceBuilders =
        createBuildersParallelFiltered allAvailableTypes resources
    
    let cloudProviderNamespace =
        match namespaces.TryGetValue(pulumiProviderName) with
        | true, Some value -> value
        | _                  -> pulumiProviderName |> toPascalCase
    
    let folder modules resourceProvider resourceBuilders =

        let ns :: subns = 
            match pulumiProviderName with
            // GCP is the only provider that seems to use inconsistent namespacing/resource provider namnes
            // ex: `gcp:privilegedaccessmanager/entitlement` belongs to `PrivilegedAccessManager`
            | "gcp" -> 
                String.split '/' resourceProvider
                |> Array.toList
            | _ -> [resourceProvider]
            
        let resourceProviderNamespace = namespaces[ns]
        
        let openNamespace =
            resourceProviderNamespace |>
            Option.orElseWith (fun () ->
                // If resourceProviderNamespace is None, it means "index"
                // Kubernetes is the only provider which overrides empty namespace with "Provider"
                match pulumiProviderName with
                | "kubernetes" -> namespaces[""]
                | _            -> None)
            |> Option.map (fun rpn ->
                    match subns with
                    | [] -> $"{cloudProviderNamespace}.{rpn}"
                    | subns -> $"""{cloudProviderNamespace}.{String.Join(".", subns)}.{rpn}""")
            |> Option.defaultValue cloudProviderNamespace
        
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
