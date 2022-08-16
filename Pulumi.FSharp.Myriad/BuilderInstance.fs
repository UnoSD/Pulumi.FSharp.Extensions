module BuilderInstance

open FSharp.Compiler.Syntax
open AstOperations
open AstInstance
open AstBuilder
open AstLet
open Myriad.Core.AstExtensions
open Core

let createBuilderInstance description typeName pTypes =
    let isYield, ops =
        Array.partition (fun x -> x.CanGenerateYield && mapOperationType (fun _ -> true) (fun _ -> false) x) pTypes
    
    let builderNames =
        isYield |>
        Array.map (function
                   | { Type = PRef t } -> String.split ':' t |> Array.last |> toCamelCase
                   | x                 -> failwith $"{x} type should not use yield")
        
    let listItem =
        sprintf " - %s"
        
    let title text =
        $"*** {text} ***"
        
    let builderNamesSection =
        match builderNames |> List.ofArray with
        | [] -> []
        | bn -> "" :: title "Nested computational expressions" :: (List.map listItem bn)
    
    let descriptionShort =
        description |> String.split '\n' |> Array.head
    
    let ccTypeName =
        if typeName = "Input" then "input'" else typeName  |> toCamelCase
    
    [ yield  descriptionShort
      yield  ""
      yield  title "Operations"
      yield! ops |> Array.map (fun x -> listItem x.OperationName) |> Array.collect (fun x -> [| ""; x |])
      yield! builderNamesSection |> List.collect (fun x -> [ ""; x ]) ] |>
    createLet ccTypeName
              (createInstance $"{typeName}Builder" SynExpr.CreateUnit)