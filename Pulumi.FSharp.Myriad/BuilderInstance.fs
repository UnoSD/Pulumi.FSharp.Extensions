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
        
    let builderNamesSection =
        match builderNames |> List.ofArray with
        | [] -> [ "</summary>" ]
        | bn -> let fn = List.map (fun x -> $" - {x}") bn
                "*** Nested computational expressions ***" :: fn @ [ "</summary>" ]
    
    let descriptionShort =
        description |> String.split '\n' |> Array.head
    
    seq {
        yield $"<summary>{descriptionShort}"
        yield  "*** Operations ***"
        yield! ops |> Array.map (fun x -> $" - {x.OperationName}")
        yield! builderNamesSection } |>
    createLet (toCamelCase typeName)
              (createInstance $"{typeName}Builder" SynExpr.CreateUnit)