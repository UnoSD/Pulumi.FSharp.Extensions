module AstYield

open AstMember
open FSharp.Compiler.Syntax
open FsAst

let createYield' isType (arg : SynPat) (args : SynExpr) (cros : SynExpr) =
    [
        SynExpr.CreateNull
        args
        if not isType then
            cros
    ] |>
    SynExpr.CreateTuple |>
    createMember "Yield" [arg.ToRcd] []
    
let createYield isType =
    let typedWildcardUnit =
        SynPatRcd.CreateTyped(SynPatRcd.CreateWild, SynType.CreateUnit) |>
        SynPatRcd.CreateParen
    
    createYield' isType (typedWildcardUnit.FromRcd)