module Program
open Pulumi.FSharp.Ops
open Pulumi.Kubernetes.Types.Inputs.Apps.V1
open Pulumi.Kubernetes.Types.Inputs.Meta.V1
open Pulumi.Kubernetes.Types.Inputs.Core.V1
open Pulumi.FSharp.Kubernetes.Apps.V1
open Pulumi.FSharp.Kubernetes.Apps.V1.Inputs
open Pulumi.FSharp.Kubernetes.Core.V1.Inputs
open Pulumi.FSharp

let infra () =

  let appLabels = inputMap ["app", input "nginx" ]

  let appDeployStd =
    DeploymentSpecArgs
      (Selector = input (LabelSelectorArgs(MatchLabels = appLabels)),
       Replicas = input 1,
       Template = input (
         PodTemplateSpecArgs
          (Metadata = input (ObjectMetaArgs(Labels = appLabels)),
           Spec = input (
              PodSpecArgs
                (Containers = 
                  inputList [
                    input (
                      ContainerArgs
                        (Name = input "nginx",
                         Image = input "nginx",
                         Ports = 
                          inputList [
                            input (
                             ContainerPortArgs
                               (ContainerPortValue = input 80))]))])))))

  let appDeployCe = 
    deployment {
      name "MyDeploy"
      deploymentSpec {
        replicas 1
        LabelSelectorArgs(
          MatchLabels = inputMap ["app", input "nginx" ] 
        )
        podTemplateSpec {
          ObjectMetaArgs(Labels = appLabels)
          podSpec {
            containers [
              input 
                (ContainerArgs(
                  Name = input "nginx",
                  Image = input "nginx",
                  Ports = inputList [
                    input 
                      (ContainerPortArgs(
                        ContainerPortValue = input 80
                      ))
                  ])
                )
            ]
          }
        }
      }
    }

  let name = 
    appDeployCe.Metadata
    |> Outputs.apply(fun (metadata) -> metadata.Name)
  dict [("name", name :> obj)]

[<EntryPoint>]
let main _ =
  Deployment.run infra
