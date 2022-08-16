module AstAttribute

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Myriad.Core.AstExtensions

let createAttribute name =
    SynAttributeList.Create(SynAttribute.Create(name))
    
let createAttributeWithArg (name : string) (arg : string) =
    let o : SynAttribute = { TypeName = LongIdentWithDots.CreateString(name)
                             ArgExpr = SynExpr.CreateParen(SynExpr.CreateConstString(arg))
                             Target = None
                             AppliesToGetterAndSetter = false
                             Range = range.Zero }
    SynAttributeList.Create(o)