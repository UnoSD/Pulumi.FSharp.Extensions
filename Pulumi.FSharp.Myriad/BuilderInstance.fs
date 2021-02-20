module BuilderInstance

open FSharp.Compiler.SyntaxTree
open AstOperations
open AstInstance
open AstBuilder
open AstLet
open FsAst
open Core

let createBuilderInstance description typeName pTypes =
    let (isYield, ops) =
        Array.partition (fun x -> x.CanGenerateYield && mapOperationType (fun _ -> true) (fun _ -> false) x) pTypes
    
    let builderNames =
        isYield |>
        Array.map (function
                   | { Type = PRef t } -> String.split ':' t |> Array.last |> toCamelCase
                   | x                 -> failwith $"{x} type should not use yield")
        
    let listItem =
        sprintf " - %s </br>"
        
    let title text =
        $"*** {text} ***"
        
    let builderNamesSection =
        match builderNames |> List.ofArray with
        | [] -> []
        | bn -> let fn = List.map listItem bn
                title "Nested computational expressions" :: fn
    
    let descriptionShort =
        description |> String.split '\n' |> Array.head
    
    seq {
        yield  "<summary>"
        yield $"{descriptionShort}</br>"
        yield  title "Operations"
        yield! ops |> Array.map (fun x -> listItem x.OperationName)
        yield! builderNamesSection
        yield  "</summary>" } |>
    createLet (toCamelCase typeName)
              (createInstance $"{typeName}Builder" SynExpr.CreateUnit)