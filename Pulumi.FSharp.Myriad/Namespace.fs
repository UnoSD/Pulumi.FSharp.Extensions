module AstNamespace

open FSharp.Compiler.SyntaxTree
open AstOpen
open FsAst

let private createNamespace' name content =
    { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong name)
                with Declarations = content }

let createNamespace modules =
    createNamespace' ("Pulumi.FSharp.Azure") ([
                 createOpen "Pulumi.FSharp"
            ] @ modules)