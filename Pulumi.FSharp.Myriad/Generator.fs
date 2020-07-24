module Pulumi.FSharp.Myriad

open AstHelpers
open AstModules
open AstConfiguration
open Myriad.Core
open System

let private getFromMap key (converter: string -> 'a) (defaultValue: 'a) map =
    if map |> Map.containsKey key
    then
        map.[key] |> converter
    else
        defaultValue

[<MyriadGenerator("Pulumi.FSharp")>]
type PulumiFSharpGenerator() =
    interface IMyriadGenerator with
        member _.Generate(namespace', fileInput) =
            let config =
                readConfig fileInput
                
            let url =
                getSchemaUrl config.["Provider"] config.["Version"]
            let useSubNamespace = config |> getFromMap "UseSubNamespace" (Boolean.Parse) false
            
            Namespace.namespace'(namespace', [
                yield  Module.open'("Pulumi.FSharp")
                
                yield! createPulumiModules url config.["Provider"] config.["Version"] useSubNamespace
            ])