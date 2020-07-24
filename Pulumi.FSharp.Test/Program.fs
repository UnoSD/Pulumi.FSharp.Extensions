module Program

open Pulumi.FSharp
open Pulumi.FSharp.Azure.Compute
open Pulumi.FSharp.Azure.Network

let infra () =
    let nic =
        networkInterface {
            name "vmnic"
        }
    
    virtualMachine {
        networkInterfaceIds nic.Id
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
    } |> ignore
    
    dict []

[<EntryPoint>]
let main _ =
  Deployment.run infra
