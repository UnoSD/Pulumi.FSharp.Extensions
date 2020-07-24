module AstYield

open AstMember
open FSharp.Compiler.SyntaxTree
open FsAst

let createYield' (arg : SynPat) args =
    [
        SynExpr.CreateNull
        args
    ] |>
    SynExpr.CreateTuple |>
    createMember "Yield" [arg.ToRcd] []
    
let createYield =
    let typedWildcardUnit =
        SynPatRcd.CreateTyped(SynPatRcd.CreateWild, SynType.CreateUnit) |>
        SynPatRcd.CreateParen
    
    createYield' (typedWildcardUnit.FromRcd)