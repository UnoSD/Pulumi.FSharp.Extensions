module Program

open System.Collections.Generic
open Azure
open Kubernetes
open Pulumi.FSharp

[<EntryPoint>]
let main _ =
  Deployment.run (fun () -> Seq.concat [azureInfra(); kubernetesInfra()] |>
                            Dictionary<string, obj> :>
                            IDictionary<string, obj>)