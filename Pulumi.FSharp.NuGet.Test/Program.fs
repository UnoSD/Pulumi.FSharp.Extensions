﻿module Program

open Pulumi.FSharp.Kubernetes.Apps.V1.Inputs
open Pulumi.FSharp.Kubernetes.Core.V1.Inputs
open Pulumi.FSharp.Kubernetes.Meta.V1.Inputs
open Pulumi.FSharp.Azure.Compute.Inputs
open Pulumi.FSharp.Azure.Network.Inputs
open Pulumi.FSharp.Azure.Compute
open Pulumi.FSharp.Azure.Network
open Pulumi.FSharp.Azure.Storage
open Pulumi.FSharp.Aws.S3.Inputs
open Pulumi.FSharp.Azure.Core
open Pulumi.FSharp.AzureAD
open Pulumi.FSharp.Assets
open Pulumi.FSharp.Aws.S3
open Pulumi.FSharp.Config
open Pulumi.FSharp.Output
open Pulumi.FSharp

let deployment = Kubernetes.Apps.V1.deployment
let container = Kubernetes.Core.V1.Inputs.container
let storageContainer = Azure.Storage.container

(*
Backup
$ echo -n "Aws Azure AzureAD Kubernetes" | xargs -I{} -n1 -d' ' bash -c 'cp Pulumi.FSharp.{}/Generated.fs $(find Pulumi.FSharp.{} -name "Generated.*.fs")'
Test difference with backup copy:
$ echo -n "Aws Azure AzureAD Kubernetes" | xargs -I{} -n1 -d' ' bash -c 'diff -qs $(find Pulumi.FSharp.{} -name "Generated.*.fs") Pulumi.FSharp.{}/Generated.fs'
*)

let infra () =
    deployment {
        name "application"

        deploymentSpec {
            replicas 1

            labelSelector { 
                matchLabels [ "app", "nginx" ]
            }

            podTemplateSpec {
                objectMeta {
                    labels [ "app", "nginx" ]
                }

                podSpec {
                    containers [
                        container {
                            name  "nginx"
                            image "nginx"
                            ports [ containerPort { containerPortValue 80 } ]
                        }
                    ]
                }
            }
        }
    }

    bucket {
        name "bucket-example"
        acl  "private"

        bucketWebsite { 
            indexDocument "index.html"
        }
    }

    group {
        name        "group-example"
        displayName "Example AAD group from Pulumi.FSharp.Extensions"
    }

    let rg =
        resourceGroup {
            name     "rg-example"
            location "West Europe"
        }

    let storage =
        account {
            resourceGroup          rg.Name
            location               rg.Location
            name                   "stexample"
            accountTier            "Standard"
            accountReplicationType "LRS"
        }

    let container =
        storageContainer { 
            storageAccountName  storage.Name
            name                "example"
            containerAccessType "private"
        }

    blob {
        name                 "file-blob"
        resourceType         "Block"
        storageContainerName container.Name
        storageAccountName   storage.Name
        source               { Path = "Program.fs" }.ToPulumiType
    }

    blob {
        name                 "url-blob"
        resourceType         "Block"
        storageContainerName container.Name
        storageAccountName   storage.Name

        source {
            Uri = "https://raw.githubusercontent.com/UnoSD/Pulumi.FSharp.Extensions/master/README.md"
        }.ToPulumiType
    }

    blob {
        name                 "archive-blob"
        resourceType         "Block"
        storageContainerName container.Name
        storageAccountName   storage.Name

        source { 
            Assets = Map.empty
                        .Add("pr.fs", File   { Path = "Program.fs" })
                        .Add("p.txt", String { Text = "text!!!!!!" }) 
        }.ToPulumiType
    }

    let vnet =
        virtualNetwork {
            name          "vnet-example"
            addressSpaces "10.0.0.0/16"
            location      rg.Location
            resourceGroup rg.Name
        }

    let subnet =
        subnet {
            name               "internal"
            resourceGroup      rg.Name
            virtualNetworkName vnet.Name
            addressPrefixes    "10.0.2.0/24"
        }

    let nic =
        networkInterface {
            name          "nic-example"
            location      rg.Location
            resourceGroup rg.Name

            ipConfigurations [ 
                networkInterfaceIpConfiguration {
                    name                       "internal"
                    subnetId                   subnet.Id
                    privateIpAddressAllocation "Dynamic"
                }
            ]
        }

    let vm =
        windowsVirtualMachine {
            name                "vm-example"
            resourceName        "vm-example"
            size                "Standard_A1_v2"
            location            rg.Location
            resourceGroup       rg.Name
            networkInterfaceIds nic.Id            

            windowsVirtualMachineOsDisk {
                name               "osdiskexample"
                caching            "ReadWrite"
                storageAccountType "Standard_LRS"
            }

            adminUsername config.["vmUser"]
            adminPassword secret.["vmPass"]

            windowsVirtualMachineSourceImageReference {
                offer     "WindowsServer"
                publisher "MicrosoftWindowsServer"
                sku       "2016-Datacenter"
                version   "latest"
            }
        }

    let secretValue =
        secretOutput { return vm.PrivateIpAddress }

    let pipCird =
        output {
            let! pip = vm.PrivateIpAddress
            
            return $"{pip}/32"
        }

    dict [ "SecretPrivateIP"     , secretValue            :> obj
           "VisiblePrivateIPCIDR", pipCird                :> obj ]

[<EntryPoint>]
let main _ = Deployment.run infra
