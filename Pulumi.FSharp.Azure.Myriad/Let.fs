module AstLet

open FSharp.Compiler.SyntaxTree
open FsAst

let createLet name expr =
    SynModuleDecl.CreateLet
        [ { SynBindingRcd.Let with
                Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString name, [])
                Expr = expr } ]