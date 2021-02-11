module Kubernetes

open Pulumi.FSharp
open Pulumi.Kubernetes.Types.Inputs.Meta.V1
open Pulumi.Kubernetes.Types.Inputs.Core.V1
open Pulumi.FSharp.Kubernetes.Apps.V1
open Pulumi.FSharp.Kubernetes.Apps.V1.Inputs
open Pulumi.FSharp.Kubernetes.Core.V1.Inputs
open Pulumi.FSharp.Kubernetes.Meta.V1.Inputs
open Pulumi.FSharp.Output

let kubernetesInfra () =
    let deployment =
        deployment {
            name "application"

            deploymentSpec {
                replicas 1

                LabelSelectorArgs(MatchLabels = inputMap [ "app", input "nginx" ])

                podTemplateSpec {

                    objectMeta { labels [ "app", input "nginx" ] }

                    podSpec {
                        containers [ ContainerArgs
                                         (Name = input "nginx",
                                          Image = input "nginx",
                                          Ports = inputList [ input (ContainerPortArgs(ContainerPortValue = input 80)) ]) ]
                    }
                }
            }
        }

    let name =
        output {
            let! md = deployment.Metadata

            return md.Name
        }

    dict [ ("Name", name :> obj) ]
