module Core

open System
open FSharp.Compiler.SyntaxTree
open FsAst

let private (|FirstLetter|) (p:string) =
    p.[0], (p.Substring(1))

let private changeInitial change value =
    let (FirstLetter(x, xs)) =
        value
    
    sprintf "%c%s" (change x) xs

let toCamelCase =
    changeInitial Char.ToLower
    
let toPascalCase =
    changeInitial Char.ToUpper
    
let createPattern name args =
    SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(name), args)
    
module String =
    let split (char : char) (value : string) = value.Split(char)
    
    let contains (subString : string) (value : string) = value.Contains(subString) 