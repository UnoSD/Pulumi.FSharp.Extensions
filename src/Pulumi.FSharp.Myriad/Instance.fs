module AstInstance

open FSharp.Compiler.Syntax
open Myriad.Core.AstExtensions

let createInstance name args =
    let identifier =
        LongIdentWithDots.CreateString name
        |> SynExpr.CreateLongIdent

    SynExpr.CreateApp(identifier, args)
