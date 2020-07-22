module AstModules

open System.IO
open System.Text.RegularExpressions
open AstHelpers
open FSharp.Data
open FSharp.Compiler.SyntaxTree
open AstBuilder
open AstLet
open Core
open AstInstance
open FSharp.Text.RegexProvider
open FsAst

let private createModuleContent properties typeName isType nameAndType =
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
    
//open FSharp.Text.RegexProvider
//
//type PossibleValues =
//    Regex<"Possible values are `(?<Value>[\w])`(?:, `(?<Value>[\w])`)? and `(?<Value>[\w])`.">
//
//let x (jValue : JsonValue) =
//    jValue.Properties() |>
//    Array.tryFind (fun (p, _) -> p = "description") |>
//    Option.map (snd >> (fun x -> x.AsString() |> PossibleValues().TypedMatches) >> (fun x -> x))
    
    let ctypes =
        types |>
        // Remove the inexisting type match
        Array.map (fst >> (function | Type x -> x | _ -> failwith "Not happening"))
    
    let getComplexType typeFullPath =
        ctypes |>
        Array.tryFind (fun t -> ("#/types/" + t.ResourceType.Value) = typeFullPath) |>
        Option.map (fun x -> "complex:" + x.ResourceType.Value) |>
        Option.defaultValue "complex" // Should be only "pulumi.json#/" type

    let nameAndType (name, (properties : (string * JsonValue) [])) =
        let tName =
            match properties |>
                  Array.tryFind (fun (p, _) -> p = "language") |>
                  Option.bind (fun (_, v) -> v.Properties() |>
                                             Array.tryFind (fun (p, _) -> p = "csharp") |>
                                             Option.map snd) |>
                  Option.map (fun v -> v.GetProperty("name").AsString()) with
            | Some name -> name
            | None      -> name
        
        let pType =
            properties |>
            Array.choose (fun (p, v) -> match p with
                                        | "type" -> v.AsString() |> Some // Array type has also "items"
                                        | "$ref" -> v.AsString() |> getComplexType |> Some
                                        (*| "description"*)
                                        | _ -> None) |>
            Array.head
        
        (tName, pType)
    
    let createType =
        (fun (x, y : JsonValue) -> nameAndType (x, y.Properties()))
    
    let create (jsonValue : JsonValue) (propertyName : string) typeName isType =
        let properties =
            jsonValue.[propertyName].Properties()
            
        createType |>
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
            Array.exists (fst >> (function | Type x when x.ResourceType.Value.StartsWith("get") |> not -> true | _ -> false))
        
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