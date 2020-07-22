module Debug

open AstBuilder
open Core

let private isDebug = false

let debugFilterTypes x =
    x |>
    if isDebug then
        Array.filter (fst >> (function | Type x -> x.ResourceType.Value = "VirtualMachineStorageOsDisk"
                                       | Resource x -> x.ResourceTypeCamelCase.Value = "virtualMachine"))
    else
        id

let debugFilterProvider (x : (string * 'a) []) : ((string * 'a) []) =
    x |>
    if isDebug then
        Array.filter (fun (provider, _) -> provider = "compute")
    else
        id