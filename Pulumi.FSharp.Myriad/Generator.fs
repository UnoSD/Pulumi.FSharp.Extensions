module Pulumi.FSharp.Myriad

open AstHelpers
open AstModules
open AstConfiguration
open Myriad.Core

[<MyriadGenerator("Pulumi.FSharp")>]
type PulumiFSharpGenerator() =
    interface IMyriadGenerator with
        member _.Generate(namespace', fileInput) =
            let config =
                readConfig fileInput
                
            let url =
                getSchemaUrl config.["Provider"] config.["Version"]
            
            Namespace.namespace'(namespace', [
                yield  Module.open'("Pulumi.FSharp")
                
                yield! createPulumiModules url config.["Provider"]
            ])