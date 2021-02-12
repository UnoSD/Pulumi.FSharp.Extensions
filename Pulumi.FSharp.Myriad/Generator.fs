module Pulumi.FSharp.Myriad

open AstConfiguration
open Myriad.Core
open AstHelpers
open AstModules

[<MyriadGenerator("Pulumi.FSharp")>]
type PulumiFSharpGenerator() =
    interface IMyriadGenerator with
        member _.Generate(context) =            
            let config =
                Ast.fromFilename context.InputFileName |>
                Async.RunSynchronously |>
                Array.head |>
                fst |>
                readConfig
                
            let provider =
                config.["Provider"]
                
            let version =
                config.["Version"]
                
            let url =
                getSchemaUrl provider version
            
            [Namespace.namespace'($"Pulumi.FSharp.{provider}", [
                yield  Module.open'("Pulumi.FSharp")
                
                yield! createPulumiModules url provider version
            ])]

        member this.ValidInputExtensions = seq { ".fs" }