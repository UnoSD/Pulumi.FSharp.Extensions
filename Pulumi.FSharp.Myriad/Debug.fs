module Debug

open AstBuilder
open Core

let private isDebug = false

let debugFilterTypes x =
    x |>
    if isDebug then
        Array.filter (fst >> (function | Type x -> List.contains x.ResourceType.Value [ "VirtualMachineStorageOsDisk"; "VirtualMachineOsProfile"; "getAccountSASPermissions"; "AccountNetworkRules"; "BucketWebsite" ]
                                       | Resource x -> List.contains x.ResourceTypePascalCase.Value [ "VirtualMachine"; "AccountNetworkRules"; "BucketWebsite" ]))
    else
        id

let debugFilterProvider (x : (string * 'a) []) : ((string * 'a) []) =
    x |>
    if isDebug then
        Array.filter (fun (provider, _) -> List.contains provider [ "compute"; "storage"; "advisor"; "s3" ])
    else
        id