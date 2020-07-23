module Pulumi.FSharp.Azure.Myriad.CommentedCode

open FSharp.Data
open FSharp.Text.RegexProvider

type PossibleValues =
    Regex<"Possible values are `(?<Value>[\w])`(?:, `(?<Value>[\w])`)? and `(?<Value>[\w])`.">

let x (jValue : JsonValue) =
    jValue.Properties() |>
    Array.tryFind (fun (p, _) -> p = "description") |>
    Option.map (snd >> (fun x -> x.AsString() |> PossibleValues().TypedMatches) >> (fun x -> x))

       
// Infix: apply :: args, but produces: apply ``::`` args
let listCons =
    SynExpr.CreateApp(SynExpr.CreateAppInfix(SynExpr.CreateLongIdent(LongIdentWithDots.CreateString("::")),
                                             SynExpr.CreateIdentString("apply")),
                      SynExpr.CreateIdentString("args"))

    
let lambdaArg =
    SynSimplePats.SimplePats([ SynSimplePat.CreateTyped(Ident.Create("args"), SynType.Bool())
                               SynSimplePat.CreateTyped(Ident.Create("f"), SynType.Bool()) ], range.Zero)
    
let lambdaArg =
    SynSimplePats.SimplePats([], range.Zero)