module Pulumi.FSharp.Azure.Myriad

open AstHelpers
open AstModules
open Myriad.Core

let private provider = "Azure"

// Version needs to match NuGet package
let private version =
    "3.11.0"

let private pulumiSchemaUrl =
    "https://raw.githubusercontent.com/pulumi/pulumi-"+
    provider.ToLower() +
    "/v" +
    version +
    "/provider/cmd/pulumi-resource-" +
    provider.ToLower() +
    "/schema.json"

[<MyriadGenerator("Pulumi.FSharp")>]
type PulumiFSharpGenerator() =
    interface IMyriadGenerator with
        member _.Generate(_, _) =
            Namespace.namespace'("Pulumi.FSharp." + provider, [
                yield Module.open'("Pulumi.FSharp")
                
                yield! createPulumiModules pulumiSchemaUrl
            ])