module Pulumi.FSharp.Azure.Myriad.CommentedCode

open FSharp.Data
open FSharp.Text.RegexProvider

type PossibleValues =
    Regex<"Possible values are `(?<Value>[\w])`(?:, `(?<Value>[\w])`)? and `(?<Value>[\w])`.">

let x (jValue : JsonValue) =
    jValue.Properties() |>
    Array.tryFind (fun (p, _) -> p = "description") |>
    Option.map (snd >> (fun x -> x.AsString() |> PossibleValues().TypedMatches) >> (fun x -> x))