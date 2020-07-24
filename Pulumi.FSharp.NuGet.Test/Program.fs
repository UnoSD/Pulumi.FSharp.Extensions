module Program

open Pulumi.FSharp
open Pulumi.FSharp.Azure.Compute
open Pulumi.FSharp.Output
open Pulumi

let infra () =
    let pwd =
        secretOutput {
            return "strongPassword"
        }

    let _ =
        virtualMachine {
            networkInterfaceIds [ "/id" ]
            name "myvm"
            vmSize "D12v3"
            virtualMachineStorageOsDisk {
                name "diskName"
                createOption "option"
            }
            resourceGroup "pippo"
            virtualMachineOsProfile {
                computerName "cname"
                adminUsername "unosd"
                adminPassword pwd
            }
        }
   
    
    dict []

[<EntryPoint>]
let main _ =
  Deployment.run infra
