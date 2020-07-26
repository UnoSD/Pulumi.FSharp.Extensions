module Program

open Pulumi.FSharp
open Pulumi.FSharp.Azure.Compute
open Pulumi.FSharp.Azure.ComputeInputs
open Pulumi.FSharp.Azure.Network
open Pulumi.FSharp.Output
open Pulumi

let infra () =
    let nic =
        networkInterface {
            name "vmnic"
        }
        
    let pwd =
        secretOutput {
            return "strongpassword"
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
            adminPassword pwd
        }
    } |> ignore
    
    let out =
        secretOutput {
            let! x = Output.Create("x")
            return x
        }
    
    dict []

[<EntryPoint>]
let main _ =
  Deployment.run infra
