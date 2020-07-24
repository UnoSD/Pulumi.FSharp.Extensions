module private Test

open Pulumi.FSharp.Azure.Compute

let private test =
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
    
    ()
