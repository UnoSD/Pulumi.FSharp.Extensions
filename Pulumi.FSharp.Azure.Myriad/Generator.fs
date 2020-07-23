module Pulumi.FSharp.Azure.Myriad

open AstHelpers
open AstModules
open Myriad.Core

let private provider = "Azure"

// Version needs to match NuGet package
let private version =
    "3.11.0"
    
//https://github.com/pulumi/pulumi-aws/blob/v2.13.1/provider/cmd/pulumi-resource-aws/schema.json?raw=true
//https://github.com/pulumi/pulumi-kubernetes/blob/v2.4.0/provider/cmd/pulumi-resource-kubernetes/schema.json?raw=true

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
                
                yield! createPulumiModules pulumiSchemaUrl
            ])