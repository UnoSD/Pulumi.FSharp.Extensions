module AstOpen

open FSharp.Compiler.SyntaxTree
open FsAst

let createOpen namespaceOrModule =
    LongIdentWithDots.CreateString(namespaceOrModule)
    |> SynModuleDecl.CreateOpen