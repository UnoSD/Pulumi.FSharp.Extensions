module AstMember

open FSharp.Compiler.SyntaxTree
open Core
open FsAst
    
let createMember' this name args attrs expr =
    let letBinding =
        { SynBindingRcd.Null with
              Pattern = createPattern (this + "." + name) args
              Expr = expr
              Attributes = attrs }

    SynMemberDefn.CreateMember(letBinding)
    
let createMember =
    createMember' "_"