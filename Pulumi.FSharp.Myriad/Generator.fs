module Pulumi.FSharp.Myriad

open AstHelpers
open AstModules
open Myriad.Core

// Parameterize these:
let private provider = "Aws" //Azure, Aws, Kubernetes
let private version = "2.13.1" // Version needs to match NuGet package 3.11.0, 2.13.1, 2.4.0
    
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
                yield  Module.open'("Pulumi.FSharp")
                
                yield! createPulumiModules pulumiSchemaUrl provider
            ])