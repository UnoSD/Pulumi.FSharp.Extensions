module Pulumi.FSharp.Config

open Pulumi

let private pulumiConfig = Lazy<Config>(fun _ -> Config())

type Config() =
    member _.Item
        with get (name) = pulumiConfig.Value.Require(name)

type SecretConfig() =
    member _.Item
        with get (name) = pulumiConfig.Value.RequireSecret(name)

// Type provider for YAML config variables
let config = Config()
let secret = SecretConfig()
