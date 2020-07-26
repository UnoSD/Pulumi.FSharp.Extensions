module Program

open Pulumi.FSharp.Azure.Compute
open Pulumi.FSharp.Config
open Pulumi.FSharp.Output
open Pulumi.FSharp
open Pulumi.Random

let infra () =
    // Add XML documentation of the properties available
    let pet =
        RandomPet("pippo", RandomPetArgs())
    
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
            return vm.PublicIpAddress
        }
    
    dict [ "SecretPublicIP", secretValue :> obj ]

[<EntryPoint>]
let main _ =
  Deployment.run infra
