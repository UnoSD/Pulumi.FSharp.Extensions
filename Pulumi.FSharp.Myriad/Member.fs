module AstMember

open FSharp.Compiler.SyntaxTree
open Core
open FsAst
    
let createMember' xmlDoc this name args attrs expr =
    let letBinding =
        { SynBindingRcd.Null with
              Pattern = createPattern (this + "." + name) args
              Expr = expr
              Attributes = attrs
              XmlDoc = match xmlDoc with | Some x -> x | None -> SynBindingRcd.Null.XmlDoc }
    
    SynMemberDefn.CreateMember(letBinding)
    
let createMember'' xmlDoc =
    createMember' xmlDoc "_"
    
let createMember =
    createMember' None "_"