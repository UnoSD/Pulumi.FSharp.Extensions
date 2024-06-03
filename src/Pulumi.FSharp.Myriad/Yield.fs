module AstYield

open AstMember
open FSharp.Compiler.Syntax
open Myriad.Core.Ast
open Myriad.Core.AstExtensions

let createYield' isType (arg: SynPat) (args: SynExpr) (cros: SynExpr) =
    [
        SynExpr.CreateNull
        args
        if not isType then
            cros
    ]
    |> SynExpr.CreateTuple
    |> createMember "Yield" arg []

let createYield isType =
    let typedWildcardUnit = SynPat.CreateTyped(SynPat.CreateWild, SynType.CreateUnit)

    createYield' isType typedWildcardUnit
