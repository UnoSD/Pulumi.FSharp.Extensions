module Program

open Pulumi.FSharp.Kubernetes.Apps.V1.Inputs
open Pulumi.FSharp.Kubernetes.Core.V1.Inputs
open Pulumi.FSharp.Kubernetes.Meta.V1.Inputs
open Pulumi.Kubernetes.Types.Inputs.Core.V1
open Pulumi.Kubernetes.Types.Inputs.Meta.V1
open Pulumi.FSharp.Azure.Compute.Inputs
open Pulumi.FSharp.Azure.Compute
open Pulumi.FSharp.Aws.S3.Inputs
open Pulumi.FSharp.AzureAD
open Pulumi.FSharp.Aws.S3
open Pulumi.FSharp.Config
open Pulumi.FSharp.Output
open Pulumi.FSharp

let deployment = Kubernetes.Apps.V1.deployment

(*
Test difference with backup copy:
$ echo -n "Aws Azure AzureAD Kubernetes" | xargs -I{} -n1 -d' ' bash -c 'diff -qs $(find Pulumi.FSharp.{} -name "Generated.*.fs") Pulumi.FSharp.{}/Generated.fs'
*)

let infra () =
    let application =
        deployment {
            name "application"            

            deploymentSpec {
                replicas 1
                
                LabelSelectorArgs(MatchLabels = inputMap [ "app", input "nginx" ])

                podTemplateSpec {

                    objectMeta { 
                        labels [ "app", input "nginx" ]
                    }

                    podSpec {
                        containers [ 
                            ContainerArgs(Name = input "nginx",
                                          Image = input "nginx",
                                          Ports = inputList [ 
                                                      input (ContainerPortArgs(ContainerPortValue = input 80))
                                                  ])
                        ]
                    }
                }
            }
        }

    let k8sAppName =
        output {
            let! md = application.Metadata
            
            return md.Name
        }

    let bucket =
        bucket {
            name "my-bucket"
            
            bucketWebsite {
                indexDocument "index.html"
            }
        }
        
    let aadGroup =
        group {
            name "aad-app"
            displayName "Test AAD group"
        }
    
    let vm =
        windowsVirtualMachine {
            name "development"
            resourceName "development"
            
            resourceGroup "Development"
            networkInterfaceIds [ config.["vmNicId"] ]
            size "Standard_D4s_v3"
            
            windowsVirtualMachineOsDisk {
                name config.["vmDiskName"]
                caching "ReadWrite"
                storageAccountType "Standard_LRS"
            }
            
            adminUsername config.["vmUser"]
            adminPassword secret.["vmPass"]
            
            windowsVirtualMachineSourceImageReference {
                offer "visualstudio2019latest"
                publisher "microsoftvisualstudio"
                sku "vs-2019-comm-latest-win10-n"
                version "latest"
            }
        }

    let secretValue =
        secretOutput {
            return vm.PrivateIpAddress
        }

    let pipCird =
        output {
            let! pip = vm.PrivateIpAddress
            
            return pip + "/32"
        }

    dict [ "SecretPrivateIP",      secretValue            :> obj
           "VisiblePrivateIPCIDR", pipCird                :> obj
           "K8sAppName",           k8sAppName             :> obj
           "AwsBucket",            bucket.WebsiteEndpoint :> obj
           "AadGroup",             aadGroup.ObjectId      :> obj ]

[<EntryPoint>]
let main _ =
  Deployment.run infra
