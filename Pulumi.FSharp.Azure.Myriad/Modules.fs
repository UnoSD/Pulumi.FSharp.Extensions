module AstModules

open System.IO
open System.Text.RegularExpressions
open AstHelpers
open FSharp.Data
open AstType
open FSharp.Compiler.SyntaxTree
open AstBuilder
open AstLet
open Core
open AstInstance
open FSharp.Text.RegexProvider
open FsAst

let private createModuleContent isType (typeName, properties, nameAndType) =
    [|
        createAzureBuilderClass isType typeName (properties |> Array.map (nameAndType))
        
        createLet (toCamelCase (typeName)) (createInstance (typeName + "Builder") SynExpr.CreateUnit)             
    |]

let private getSchemaFromCacheOrUrl schemaUrl =
    if File.Exists("schema.json") then
        File.ReadAllText("schema.json")
    else
        let json =
            Http.RequestString(schemaUrl)
        
        #if DEBUG
        File.WriteAllText("schema.json", json)
        #endif

        json
// "azure:compute/virtualMachine:VirtualMachine"
// CloudProvider - Always the same for each schema (azure here)
type ResourceInfoProvider =
    Regex<"(?<CloudProvider>\w+):(?<ResourceProviderNamespace>\w+)/(?<ResourceTypeCamelCase>\w+):(?<ResourceTypePascalCase>\w+)">

type TypeInfoProvider =
    Regex<"(?<CloudProvider>\w+):(?<ResourceProviderNamespace>\w+)/(?<ResourceType>\w+):(?<ResourceType2>\w+)">

let private resourceInfo =
    ResourceInfoProvider(RegexOptions.Compiled)

let private typeInfo =
    TypeInfoProvider(RegexOptions.Compiled)
    
type BuilderType =
    | Type of TypeInfoProvider.MatchType
    | Resource of ResourceInfoProvider.MatchType
    
let createPulumiModules schemaUrl =
    let schema =
        getSchemaFromCacheOrUrl schemaUrl |>
        JsonValue.Parse
    
    let pulumiProviderName =
        schema.["name"].AsString()
    
    let types =
        schema.["types"].Properties() |>
        Array.map (fun (type', jv) -> typeInfo.TypedMatch(type') |> Type, jv)
    
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
    
    let createBuilders (typeInfo, jsonValue) =
        match typeInfo with
        | Type t     -> createType true schema (t.Value, jsonValue) |> createModuleContent true
        | Resource r -> createType false schema (r.Value, jsonValue) |> createModuleContent false
    
    let createProviderModule (providerName, builders) =
        let moduleName =
            match namespaces |> Map.tryFind providerName with
            | Some ns -> ns
            | None    -> sprintf "Unable to find namespace %s" providerName |> failwith
        
        let types =
            builders |>
            // Debug filter
            //Array.filter (fst >> (function | Type x -> x.ResourceType.Value = "VirtualMachineStorageOsDisk"
            //                               | Resource x -> x.ResourceTypeCamelCase.Value = "virtualMachine")) |>
            Array.filter (fst >> (function | Type x when x.ResourceType.Value.StartsWith("get") ||
                                                         // Fix this
                                                         x.ResourceType.Value = "AccountNetworkRules" -> false
                                           | _ -> true)) |>
            Array.collect createBuilders |>
            List.ofArray
        
        let hasTypes =
            builders |>
            Array.exists (fst >> (function | Type _ -> true | _ -> false))
        
        let openInputs =
            match hasTypes with
            | true  -> [ Module.open'("Pulumi." + namespaces.[pulumiProviderName] + "." + moduleName + ".Inputs") ]
            | false -> []
            
        Module.module'(moduleName,
                       Module.open'("Pulumi." + namespaces.[pulumiProviderName] + "." + moduleName)
                       :: openInputs
                       @  types)
    
    let providerModules =
        Array.concat [ types; resources ] |>
        Array.groupBy resourceProvider |>
        // Debug filter
        //Array.filter (fun (x, _) -> x = "compute") |>
        Array.filter (fun (_, b) -> not <| Array.isEmpty b) |>
        Array.filter (fun (x, _) -> [ "config"; "index" ] |> List.contains x |> not) |>
        Array.map createProviderModule
    
    providerModules |> List.ofArray