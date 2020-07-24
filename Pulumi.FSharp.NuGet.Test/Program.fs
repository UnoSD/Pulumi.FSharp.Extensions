module Program

open Pulumi.FSharp
open Pulumi.FSharp.Azure.Compute

let infra () =
    let vm =
        virtualMachine {
            networkInterfaceIds [ input "/id" ]
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
                adminPassword "strongpassword"
            }
        }

    let _ =
        virtualMachine {
            vmSize "size1"
            virtualMachineStorageOsDisk {
                name "disk1"
            }
            name "name1"
        }
        
    let _ =
        virtualMachine {
            virtualMachineStorageOsDisk {
                name "disk2"
            }
            vmSize "size2"
            name "name2"
        }
        
    let _ =
        virtualMachine {
            name "name3"
            vmSize "size3"
            virtualMachineStorageOsDisk {
                name "disk3"
            }
        }
    
    dict []

[<EntryPoint>]
let main _ =
  Deployment.run infra
