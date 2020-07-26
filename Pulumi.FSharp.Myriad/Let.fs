module AstLet

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.XmlDoc
open FsAst

let createLet name expr (docs : string seq) =
    SynModuleDecl.CreateLet
        [ { SynBindingRcd.Let with
                Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString name, [])
                Expr = expr
                XmlDoc = PreXmlDoc.Create(docs) } ]