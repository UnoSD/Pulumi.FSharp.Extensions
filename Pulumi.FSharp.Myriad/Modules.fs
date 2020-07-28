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

let typesToIgnorePerProvider = 
    [
        ("kubernetes", [
            // Json
            "JSONSchemaProps"
            "CustomResourceSubresources"
            "ControllerRevision"

            // Unknown
            "DeploymentRollback"
            "Scale"
            "ScaleStatus"
            "ScaleSpec"
            "TokenRequestStatus"
            "TokenReviewStatus"
            "UserInfo"
            "NonResourceRule"
            "ResourceRule"
            "SubjectAccessReviewStatus"
            "SubjectRulesReviewStatus"
            "APIGroup"
            "APIGroupList"
            "APIResource"
            "APIResourceList"
            "APIVersions"
            "DeleteOptions"
            "GroupVersionForDiscovery"
            "Preconditions"
            "ServerAddressByClientCIDR"
            "WatchEvent"
            "Eviction"
            "PriorityClass"
            "PriorityClassList"
            "Info"

            // Naming collission on args
            // "Container"
            // "EphemeralContainer"

            // Duplicate yield
            "MutatingWebhook"
            "ValidatingWebhook"
            "CSIPersistentVolumeSource"
            "NodeConfigStatus"
            "VolumeAttachmentStatus"
            "HorizontalPodAutoscalerBehavior"
            "Event"
            "ContainerStatus"
            "NetworkPolicyPeer"
            "Lifecycle"
        ])
    ]
    |> Map.ofList

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
    
let private createModule name namespacesToOpen types =
    Module.module'(name, [
        yield! namespacesToOpen |> List.map (Module.open')
//        yield  Module.open'("Pulumi." + namespace' + "." + name)

        yield! types
    ])

let createPulumiModules schemaUrl providerName version useSubNamespace =
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
        typedMatches "resources" resourceInfo Resource id
    
    let fullProviderName (builder, _) =
        match builder with
        | Type t     -> t.CompleteMatch.Value
        | Resource r -> r.CompleteMatch.Value

    let resourceProvider ((typeInfo, _), _) =
        match typeInfo with
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

    let getNamespace typeInfo =
        let (resourceProviderNamespace, subNamespace) =
            match typeInfo with
            | Type t     -> (t.ResourceProviderNamespace.Value, t.SubNamespace.Value)
            | Resource r -> (r.ResourceProviderNamespace.Value, r.SubNamespace.Value)

        let lookupName =
            if useSubNamespace
            then sprintf "%s/%s" resourceProviderNamespace subNamespace
            else resourceProviderNamespace

        match namespaces |> Map.tryFind lookupName with
        | Some ns -> ns
        | None    -> sprintf "Unable to find namespace %s, %A" lookupName useSubNamespace |> failwith

    let getModuleNames typeInfo =
        let fullNamespace = typeInfo |> getNamespace
        let namespaceParts = fullNamespace.Split(".")
        let moduleName = fullNamespace.Split(".").[0]
        let submoduleNameOption = 
            if namespaceParts.Length > 1 then namespaceParts.[1] |> Some else None
        (moduleName, submoduleNameOption)

    let enrichWithModuleNames (typeInfo, builder) =
        let moduleNames = getModuleNames typeInfo
        (moduleNames, (typeInfo, builder))

    let createBuilders moduleName (submoduleNameOption, builderInfos) =
        let builders =
            builderInfos |>
            Array.Parallel.collect (
                fun (_, (typeInfo, jsonValue : JsonValue)) -> 
                    match typeInfo with
                    | Type t     -> create jsonValue "properties" t.ResourceType.Value true
                    | Resource r -> create jsonValue "inputProperties" r.ResourceTypePascalCase.Value false
                )
        (moduleName, submoduleNameOption), builders

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
    
    let typesToIgnore = 
        typesToIgnorePerProvider 
        |> Map.tryFind pulumiProviderName
        |> Option.defaultValue []

    let createBuildersParallelFiltered typesOrResources definedTypes =
        let ignoreBasedOnList valuesToCheck t = valuesToCheck |> List.contains t

        let filterOnBuilderType builderType =
            match builderType with 
            | Type x ->
                not (
                    x.ResourceType.Value |> ignoreBasedOnList typesToIgnore ||
                    x.CompleteMatch.Value |> ignoreBasedOnList definedTypes)
            | Resource x -> 
                not (
                    x.ResourceTypePascalCase.Value |> ignoreBasedOnList typesToIgnore ||
                    x.CompleteMatch.Value |> ignoreBasedOnList definedTypes)

        typesOrResources |>
        Array.map enrichWithModuleNames |>
        Array.groupBy (fst >> fst) |>
        filters |>
        Map.ofArray |>
        Map.map (
            fun moduleName typesOrResources -> 
                typesOrResources |>
                debugFilterTypes |>
                Array.filter (snd >> fst >> filterOnBuilderType) |>
                Array.groupBy (fst >> snd) |>
                Array.Parallel.map (createBuilders moduleName >> (fun ((_, subNs), builders) -> subNs, builders)) |>
                Map.ofArray)
        
    let resourceBuilders =
        createBuildersParallelFiltered resources []

    let resourceTypeLookup =
        resources
        |> Array.map (fst >> (function 
            | Type x -> x.CompleteMatch.Value
            | Resource x -> x.CompleteMatch.Value
            ))
        |> List.ofArray

    let typeBuilders =
        createBuildersParallelFiltered types resourceTypeLookup
        
    let providerNamespaceName =
        if namespaces |> Map.containsKey pulumiProviderName
        then namespaces.[pulumiProviderName]
        else pulumiProviderName |> toPascalCase

    let inputTypeNamespace ns =
        if useSubNamespace
        then sprintf "Pulumi.%s.Types.Inputs.%s" providerNamespaceName ns
        else sprintf "Pulumi.%s.%s.Inputs" providerNamespaceName ns

    Map.fold (
        fun outerModules provider resourceBuilders ->
            let innerModules =
                resourceBuilders |>
                Map.toSeq |>
                Seq.collect (
                    fun (innerProvider, resourceBuilders') ->
                        let moduleContent =
                            let typesModule =
                                typeBuilders |>
                                Map.tryFind provider |>
                                Option.bind (fun innerMap ->
                                    innerMap |>
                                    Map.tryFind innerProvider |>
                                    Option.bind (
                                        fun providerTypeBuilders -> 
                                            if Array.isEmpty providerTypeBuilders
                                            then None
                                            else Some providerTypeBuilders) |>
                                    Option.map (
                                        fun providerTypeBuilders -> 
                                            [|createModule "Inputs" [] providerTypeBuilders|]
                                    )
                                ) |>
                                Option.defaultValue [||]

                            Array.append typesModule resourceBuilders'
                        
                        match innerProvider with
                        | None -> moduleContent
                        | Some subNs ->
                            let relativeNs = sprintf "%s.%s" provider subNs

                            moduleContent |>
                            List.ofArray |>
                            (fun mc -> 
                                let subModuleNamespace = sprintf "Pulumi.%s.%s.%s" providerNamespaceName provider subNs
                                let inputsNamespace = inputTypeNamespace relativeNs
                                [| createModule subNs [subModuleNamespace; inputsNamespace] mc |])
                )
            let providerNamespace = sprintf "Pulumi.%s.%s" providerNamespaceName provider
            let inputsNamespace = inputTypeNamespace provider
            (createModule provider [providerNamespace; inputsNamespace] innerModules) :: outerModules)
            []
            resourceBuilders
// =======
//     let createProviderModule (providerName, builders) =
//         let typesToIgnore = 
//             typesToIgnorePerProvider 
//             |> Map.tryFind pulumiProviderName
//             |> Option.defaultValue []
//         let namespacesWithTypes =
//             builders |>
//             debugFilterTypes |>
//             Array.filter (fst >> (function 
//                 | Type x -> 
//                     not (
//                         x.ResourceType.Value.StartsWith("get", StringComparison.Ordinal) ||
//                         // Fix this
//                         x.ResourceType.Value = "AccountNetworkRules" || 
//                         typesToIgnore |> List.contains x.ResourceType.Value
//                     )
//                 | Resource x -> 
//                     not (typesToIgnore |> List.contains x.ResourceTypePascalCase.Value))) |>
//             Array.groupBy expandModuleInfo |>
//             Array.map createBuilders

//         let hasTypes =
//             builders |>
//             Array.exists (fst >> (function | Type x when x.ResourceType.Value.StartsWith("get", StringComparison.Ordinal) |> not -> true | _ -> false))
        
//         let providerNamespaceName =
//             if namespaces |> Map.containsKey pulumiProviderName
//             then namespaces.[pulumiProviderName]
//             else pulumiProviderName |> toPascalCase

//         let inputTypeNamespace ns =
//             if useSubNamespace
//             then sprintf "Pulumi.%s.Types.Inputs.%s" providerNamespaceName ns
//             else sprintf "Pulumi.%s.%s.Inputs" providerNamespaceName ns

//         let openInputs ns =
//             if hasTypes then
//                 [ Module.open'(inputTypeNamespace ns) ]
//             else
//                 []
        
//         let res =
//             namespacesWithTypes |>
//             Array.groupBy (fst >> fst) |>
//             Array.map (fun (outerModuleName, outerBuilderGroup) -> 
//                 let outerModules =
//                     outerBuilderGroup
//                     |> List.ofArray
//                     |> List.groupBy (fst >> snd)
//                     |> List.collect (fun (innerModule, innerBuilderGroup) ->
//                         let createModuleContent relativeNs =
//                             let resourceNs = sprintf "Pulumi.%s.%s" providerNamespaceName relativeNs
//                             [
//                                 yield  Module.open'(resourceNs)
//                                 yield! openInputs relativeNs
//                                 yield! (innerBuilderGroup |> Seq.collect snd)
//                             ]

//                         match innerModule with
//                         | Some m -> 
//                             sprintf "%s.%s" outerModuleName m
//                             |> createModuleContent
//                             |> (fun moduleContent -> [ Module.module'(m, moduleContent) ])
//                         | None -> 
//                             sprintf "%s" outerModuleName
//                             |> createModuleContent
//                     )
//                 Module.module'(outerModuleName, outerModules))
//         res

//     Array.concat [ resources; types ] |>
//     Array.groupBy fullProviderName |>
//     Array.map (snd >> Array.head) |>
//     Array.groupBy resourceProvider  |>
//     debugFilterProvider |>
//     Array.filter (fun (_, builders) -> not <| Array.isEmpty builders) |>
//     Array.filter (fun (provider, _) -> not <| List.contains provider [ "config"; "index"; "" ]) |>
//     Array.Parallel.collect createProviderModule
// >>>>>>> Make kubernetes compile
