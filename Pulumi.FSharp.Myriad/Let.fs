module AstLet

open FSharp.Compiler.Syntax
open FSharp.Compiler.Xml
open FsAst

let createLet name expr (docs : string list) =
    SynModuleDecl.CreateLet
        [ { SynBindingRcd.Let with
                Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString name, [])
                Expr = expr
                XmlDoc = PreXmlDoc.Create(docs) } ]