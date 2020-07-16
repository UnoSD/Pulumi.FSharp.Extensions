module Pulumi.FSharp.Azure.Myriad

open System
open Myriad.Core
open AstFile

[<RequireQualifiedAccess>]
module Generator =
    type PulumiAttribute() =
        inherit Attribute()

[<MyriadGenerator("example1")>]
type Example1Gen() =
    interface IMyriadGenerator with
        member __.Generate(_, _) =
            createFile ()