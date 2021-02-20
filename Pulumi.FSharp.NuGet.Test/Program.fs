module Program

open Pulumi.FSharp.Kubernetes.Apps.V1.Inputs
open Pulumi.FSharp.Kubernetes.Core.V1.Inputs
open Pulumi.FSharp.Kubernetes.Meta.V1.Inputs
open Pulumi.FSharp.Azure.Compute.Inputs
open Pulumi.FSharp.Azure.Compute
open Pulumi.FSharp.Azure.Storage
open Pulumi.FSharp.Aws.S3.Inputs
open Pulumi.FSharp.AzureAD
open Pulumi.FSharp.Assets
open Pulumi.FSharp.Aws.S3
open Pulumi.FSharp.Config
open Pulumi.FSharp.Output
open Pulumi.FSharp

let deployment = Kubernetes.Apps.V1.deployment
let container = Kubernetes.Core.V1.Inputs.container

(*
Backup
$ echo -n "Aws Azure AzureAD Kubernetes" | xargs -I{} -n1 -d' ' bash -c 'cp Pulumi.FSharp.{}/Generated.fs $(find Pulumi.FSharp.{} -name "Generated.*.fs")'
Test difference with backup copy:
$ echo -n "Aws Azure AzureAD Kubernetes" | xargs -I{} -n1 -d' ' bash -c 'diff -qs $(find Pulumi.FSharp.{} -name "Generated.*.fs") Pulumi.FSharp.{}/Generated.fs'
*)



let infra () =
    
    let _ =
        blob {
            name "storageblob1"
            storageContainerName "cont"
            storageAccountName "storage"
            resourceType "Block"
            source { Path = "Program.fs" }.ToPulumiType
        }
        
    let _ =
        blob {
            name "storageblob2"
            storageContainerName "cont"
            storageAccountName "storage"
            resourceType "Block"
            source { Uri = "https://raw.githubusercontent.com/UnoSD/Pulumi.FSharp.Extensions/master/README.md" }.ToPulumiType
        }
        
    let _ =
        blob {
            name "storageblob3"
            storageContainerName "cont"
            storageAccountName "storage"
            resourceType "Block"
            source { Assets = Map.empty
                                 .Add("name" , File { Path = "Program.fs" })
                                 .Add("name2", String { Text = "text" }) }.ToPulumiType
        }
    
    let application =
        deployment {
            name "application"            

            deploymentSpec {
                replicas 1
                
                labelSelector {
                    matchLabels [ "app", input "nginx" ]
                }

                podTemplateSpec {                    
                    objectMeta { 
                        labels [ "app", input "nginx" ]
                    }

                    podSpec {
                        containers [                            
                            container {
                                name "nginx"
                                image "nginx"
                                ports [ containerPort { containerPortValue 80 } ]
                            }
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
