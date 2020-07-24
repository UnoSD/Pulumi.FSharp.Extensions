module AstInstance

open FSharp.Compiler.SyntaxTree
open FsAst

let createInstance name args =
    let identifier =
        LongIdentWithDots.CreateString name |>
        SynExpr.CreateLongIdent
        
    SynExpr.CreateApp(identifier, args)