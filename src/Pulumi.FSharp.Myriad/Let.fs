module AstLet

open FSharp.Compiler.Syntax
open FSharp.Compiler.Xml
open Myriad.Core.Ast
open Myriad.Core.AstExtensions

let createLet name expr (docs: string list) =
    SynModuleDecl.CreateLet [
        SynBinding.Let(
            pattern = SynPat.CreateLongIdent(LongIdentWithDots.CreateString name, []),
            expr = expr,
            xmldoc = PreXmlDoc.Create(docs)
        )
    ]
