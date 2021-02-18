module BuilderInstance

open AstOperations
open FSharp.Compiler.SyntaxTree
open AstInstance
open AstLet
open FsAst
open Core

let createBuilderInstance typeName pTypes =
    seq {
        yield "*** Available properties ***"
        yield ""
        yield "When names are available on the resource,"
        yield "**resourceName** maps to the name of the"
        yield "provider resource, **name** maps to the"
        yield "Pulumi name"
        yield ""
        yield! pTypes |> Array.map (fun x -> x.Name) } |>
    createLet (toCamelCase typeName)
              (createInstance $"{typeName}Builder" SynExpr.CreateUnit)