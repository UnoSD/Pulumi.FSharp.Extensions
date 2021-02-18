module BuilderInstance

open FSharp.Compiler.SyntaxTree
open AstOperations
open AstInstance
open AstBuilder
open AstLet
open FsAst
open Core

let createBuilderInstance typeName pTypes =
    let (isYield, ops) =
        Array.partition (fun x -> x.CanGenerateYield && mapOperationType (fun _ -> true) (fun _ -> false) x) pTypes
    
    let builderNames =
        isYield |>
        Array.map (function
                   | { Type = PRef t } -> String.split ':' t |> Array.last |> toCamelCase
                   | x                 -> failwith $"{x} type should not use yield")
        
    let builderNamesSection =
        match builderNames |> List.ofArray with
        | [] -> []
        | bn -> "" :: "*** Nested properties ***" :: bn
    
    seq {
        yield "*** Available properties ***"
        yield ""
        yield "When names are available on the resource,"
        yield "**resourceName** maps to the name of the"
        yield "provider resource, **name** maps to the"
        yield "Pulumi name"
        yield ""
        yield "*** Operations ***"
        yield! ops |> Array.map (fun x -> x.OperationName)
        yield! builderNamesSection } |>
    createLet (toCamelCase typeName)
              (createInstance $"{typeName}Builder" SynExpr.CreateUnit)