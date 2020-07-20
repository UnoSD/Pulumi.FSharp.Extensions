module Pulumi.FSharp.Azure.Test

open Pulumi.FSharp.Azure.Compute

let test =
    let vm =
        virtualMachine {
            name "nam"
            virtualMachineStorageOsDisk {
                name ""
            }
            vmSize ""
        }
    
    0