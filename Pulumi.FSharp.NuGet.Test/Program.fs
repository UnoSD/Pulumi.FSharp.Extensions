module Program

open Pulumi.FSharp.Azure.Compute.Inputs
open Pulumi.FSharp.Azure.Compute
open Pulumi.FSharp.Config
open Pulumi.FSharp.Output
open Pulumi.FSharp

(*
Test difference with backup copy:
$ echo -n "Aws Azure AzureAD Kubernetes" | xargs -I{} -n1 -d' ' bash -c 'diff -qs $(find Pulumi.FSharp.{} -name "Generated.*.fs") Pulumi.FSharp.{}/Generated.fs'
*)

let infra () =
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

    dict [ "SecretPrivateIP",      secretValue :> obj
           "VisiblePrivateIPCIDR", pipCird     :> obj ]

[<EntryPoint>]
let main _ =
  Deployment.run infra
