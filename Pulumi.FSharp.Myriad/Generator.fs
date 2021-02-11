module Pulumi.FSharp.Myriad

open AstHelpers
open AstModules
open AstConfiguration
open Core
open Myriad.Core

[<MyriadGenerator("Pulumi.FSharp")>]
type PulumiFSharpGenerator() =
    interface IMyriadGenerator with
        member _.Generate(namespace', fileInput) =
            let config =
                readConfig fileInput
                
            let url =
                getSchemaUrl config.["Provider"] config.["Version"]
                
            let useSubNamespace =
                config |> getOrDefault "UseSubNamespace" bool.Parse false
            
            Namespace.namespace'(namespace', [
                yield  Module.open'("Pulumi.FSharp")
                
                yield! createPulumiModules url config.["Provider"] config.["Version"] useSubNamespace
            ])