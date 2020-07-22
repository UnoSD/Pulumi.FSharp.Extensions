module AstModules

open System.IO
open AstHelpers
open FSharp.Data
open FSharp.Compiler.SyntaxTree
open AstBuilder
open AstLet
open Core
open AstInstance
open FsAst

let private createModuleContent (properties : (string * JsonValue) []) typeName isType types =
    [|
        createAzureBuilderClass isType typeName properties types
        
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
    
let createPulumiModules schemaUrl =
    let schema =
        getSchemaFromCacheOrUrl schemaUrl |>
        JsonValue.Parse
    
    let pulumiProviderName =
        schema.["name"].AsString()
    
    let typeMatches =
        schema.["types"].Properties() |>
        Array.map (fun (type', jv) -> typeInfo.TypedMatch(type'), jv)
    
    let types =
        typeMatches |>
        Array.map (fun (typedMatch, jsonValue) -> (Type typedMatch, jsonValue))
    
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
    
    let create (jsonValue : JsonValue) (propertyName : string) typeName isType =
        let properties =
            jsonValue.[propertyName].Properties()
            
        createModuleContent properties typeName isType typeMatches
    
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