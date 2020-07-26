module Debug

open AstBuilder
open Core

let private isDebug = false

let debugFilterTypes x =
    x |>
    if isDebug then
        Array.filter (fst >> (function | Type x -> List.contains x.ResourceType.Value [ "WindowsVirtualMachineOsDisk"; "WindowsVirtualMachineSourceImageReference"; "getAccountSASPermissions"; "AccountNetworkRules" ]
                                       | Resource x -> List.contains x.ResourceTypePascalCase.Value [ "WindowsVirtualMachine"; "AccountNetworkRules"; "NetworkInterface" ]))
    else
        id

let debugFilterProvider (x : (string * 'a) []) : ((string * 'a) []) =
    x |>
    if isDebug then
        Array.filter (fun (provider, _) -> List.contains provider [ "compute"; "storage"; "advisor"; "s3"; "apigateway"; "network" ])
    else
        id