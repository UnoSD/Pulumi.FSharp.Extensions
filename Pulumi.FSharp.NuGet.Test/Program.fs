module Program

open Pulumi.FSharp.Kubernetes.Apps.V1.Inputs
open Pulumi.FSharp.Kubernetes.Core.V1.Inputs
open Pulumi.FSharp.Kubernetes.Meta.V1.Inputs
open Pulumi.FSharp.AzureNative.Compute.Inputs
open Pulumi.FSharp.AzureNative.Network.Inputs
open Pulumi.FSharp.AzureNative.Resources
open Pulumi.FSharp.AzureNative.Storage
open Pulumi.FSharp.AzureNative.Network
open Pulumi.FSharp.AzureNative.Compute
open Pulumi.FSharp.Aws.S3.Inputs
open Pulumi.AzureNative.Storage
open Pulumi.AzureNative.Compute
open Pulumi.FSharp.AzureAD
open Pulumi.FSharp.Outputs
open Pulumi.FSharp.Assets
open Pulumi.FSharp.Aws.S3
open Pulumi.FSharp.Config
open Pulumi.FSharp
open Pulumi.FSharp.Gcp

let deployment = Kubernetes.Apps.V1.deployment
let container = Kubernetes.Core.V1.Inputs.container
let nicSubnet = AzureNative.Network.Inputs.subnet
let sku = AzureNative.Storage.Inputs.sku
let networkProfile = AzureNative.Compute.Inputs.networkProfile
let azureLegacyStorageAccount = Azure.Storage.account

(*
Backup
$ echo -n "Aws Azure AzureNative AzureAD Kubernetes" | xargs -I{} -n1 -d' ' bash -c 'cp Pulumi.FSharp.{}/Generated.fs $(find Pulumi.FSharp.{} -name "Generated.*.fs")'
Test difference with backup copy:
$ echo -n "Aws Azure AzureNative AzureAD Kubernetes" | xargs -I{} -n1 -d' ' bash -c 'diff -qs $(find Pulumi.FSharp.{} -name "Generated.*.fs") Pulumi.FSharp.{}/Generated.fs'
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

    azureLegacyStorageAccount {
        resourceGroup          rg.Name
        location               rg.Location
        name                   "stlegacyexample"
        accountTier            "Standard"
        accountReplicationType "LRS"
    }

    let storage =
        storageAccount {
            resourceGroup          rg.Name
            location               rg.Location
            name                   "stexample"            
            sku                    { name "LRS" }            
            kind                   Kind.StorageV2
        }

    let container =
        blobContainer { 
            accountName   storage.Name
            resourceGroup rg.Name
            name         "example"
            
            PublicAccess.None
        }

    blob {
        name          "file-blob"
        containerName container.Name
        resourceGroup rg.Name
        accountName   storage.Name
        source        { Path = "Program.fs" }.ToPulumiType
        
        BlobType.Block
    }

    blob {
        name                 "url-blob"
        containerName container.Name
        resourceGroup rg.Name
        accountName   storage.Name

        source {
            Uri = "https://raw.githubusercontent.com/UnoSD/Pulumi.FSharp.Extensions/master/README.md"
        }.ToPulumiType
        
        BlobType.Block
    }

    blob {
        name          "archive-blob"
        containerName container.Name
        resourceGroup rg.Name
        accountName   storage.Name

        source { 
            Assets = Map.empty
                        .Add("pr.fs", File   { Path = "Program.fs" })
                        .Add("p.txt", String { Text = "text!!!!!!" }) 
        }.ToPulumiType
        
        BlobType.Block
    }

    let vnet =
        virtualNetwork {
            name          "vnet-example"
            location      rg.Location
            resourceGroup rg.Name            
            addressSpace  { addressPrefixes "10.0.0.0/16" }
        }

    let subnet =
        subnet {
            name               "internal"
            resourceGroup      rg.Name
            virtualNetworkName vnet.Name
            addressPrefix      "10.0.2.0/24"
        }

    let nic =
        networkInterface {
            name          "nic-example"
            location      rg.Location
            resourceGroup rg.Name

            ipConfigurations [ 
                networkInterfaceIPConfiguration {
                    name                      "internal"
                    privateIPAllocationMethod "Dynamic"                    
                    nicSubnet                 { id subnet.Id }
                }
            ]
        }
    
    virtualMachine {
        name                "vm-example"
        vmName              "vm-example"
        location            rg.Location
        resourceGroup       rg.Name
        
        hardwareProfile {
            vmSize "Standard_A1_v2"    
        }
        
        networkProfile {
            networkInterfaces [
                networkInterfaceReference { id nic.Id }
            ]
        }
        
        oSProfile {
            adminUsername config.["vmUser"]
            adminPassword secret.["vmPass"]
        }
        
        storageProfile {
            oSDisk {
                name                  "osdiskexample"
                createOption          DiskCreateOptionTypes.FromImage
                managedDiskParameters { storageAccountType "Standard_LRS" }
                
                CachingTypes.ReadWrite
            }
            
            imageReference {
                offer     "WindowsServer"
                publisher "MicrosoftWindowsServer"
                sku       "2016-Datacenter"
                version   "latest"
            }
        }
    }

    let privateIp =
        output {
            let! ipConfigs = nic.IpConfigurations
            
            return ipConfigs.[0].PrivateIPAddress
        }

    let secretPipCird =
        secretOutput {
            let! pip = privateIp
            
            return $"{pip}/32"
        }

    dict [ "VisiblePrivateIP"   , privateIp     :> obj
           "SecretPrivateIPCIDR", secretPipCird :> obj ]

[<EntryPoint>]
let main _ = Deployment.run infra
