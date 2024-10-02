module Pulumi.FSharp.Config

open Pulumi

let private pulumiConfig = lazy Config()

type Config() =
    member _.Item
        with get (name) = pulumiConfig.Value.Require(name)

    member _.get<'T> (key: string) =
        pulumiConfig.Value.RequireObject<'T>(key)

    member _.tryGet<'T when 'T: null> (key: string) =
        pulumiConfig.Value.GetObject<'T>(key)
        |> Option.ofObj

type SecretConfig() =
    member _.Item
        with get (name) = pulumiConfig.Value.RequireSecret(name)

    member _.get<'T> (key: string) =
        pulumiConfig.Value.RequireSecretObject<'T>(key)

    member _.tryGet<'T> (key: string) =
        pulumiConfig.Value.GetSecretObject<'T>(key)
        |> Option.ofObj

// Type provider for YAML config variables
let config = Config()
let secret = SecretConfig()
