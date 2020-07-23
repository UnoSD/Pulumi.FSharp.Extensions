module Debug

open AstBuilder
open Core

let private isDebug = false

let debugFilterTypes x =
    x |>
    if isDebug then
        Array.filter (fst >> (function | Type x -> List.contains x.ResourceType.Value [ "VirtualMachineStorageOsDisk"; "getAccountSASPermissions"; "AccountNetworkRules" ]
                                       | Resource x -> List.contains x.ResourceTypeCamelCase.Value [ "virtualMachine"; "accountNetworkRules" ]))
    else
        id

let debugFilterProvider (x : (string * 'a) []) : ((string * 'a) []) =
    x |>
    if isDebug then
        Array.filter (fun (provider, _) -> List.contains provider [ "compute"; "storage"; "advisor" ])
    else
        id