module Debug

open AstBuilder
open Core

type private Filter<'a> =
    | Include of 'a list
    | Exclude of 'a list

type private Filters = {
        Providers: Filter<string>
        Resources: Filter<string>
        Types: Filter<string>
    }

let private azureFilters = {
        Types     = Include [
            "WindowsVirtualMachineOsDisk"
            "ProviderFeaturesKeyVault"
            "WindowsVirtualMachineSourceImageReference"
            "getAccountSASPermissions"
            "AccountNetworkRules"
        ]
        Resources = Include [
            "WindowsVirtualMachine"
            "AccountNetworkRules"
            "NetworkInterface"
        ]
        Providers = Include [
            "compute"
            "storage"
            "network"
            "index"
        ]                        
    }

let private awsFilters = {
        Types     = Include [ "AccessPointVpcConfiguration" ]
        Resources = Include [ "Bucket" ]
        Providers = Include [ "s3" ]                        
    }

let private kubernetesFilters = {
        Types     = Include [ "VolumeAttachment" ]
        Resources = Include [ "VolumeAttachment" ]
        Providers = Include [ "storage" ]                        
    }

let private merge left right =
    match left, right with
    | Include li, Include ri -> li @ ri |> Include
    | Exclude le, Include re -> le @ re |> Exclude
    | _                      -> failwith "Using include and exclude debug filters together is not supported"

let private join filtersList =
    let reduce select =
        filtersList |> List.map select |> List.reduce merge
    
    {
        Types     = reduce (fun f -> f.Types)
        Resources = reduce (fun f -> f.Resources)
        Providers = reduce (fun f -> f.Providers)
    }

let private isDebug = false
let private filters = join [ azureFilters; awsFilters; kubernetesFilters ]

let private typeSelector builderType =
    match (builderType, filters.Types, filters.Resources) with
    | (Type     t, Include ts, _         ) -> List.contains t.ResourceType.Value ts
    | (Type     t, Exclude ts, _         ) -> not <| List.contains t.ResourceType.Value ts
    | (Resource r, _         , Include rs) -> List.contains r.ResourceType.Value rs
    | (Resource r, _         , Exclude rs) -> not <| List.contains r.ResourceType.Value rs

let private providerSelector provider =
    match filters.Providers with
    | Include ps -> List.contains provider ps
    | Exclude ps -> not <| List.contains provider ps

let private debugTupleArrayFilter filter values =
    values |>
    if isDebug then
        Array.filter (fst >> filter)
    else
        id

let debugFilterTypes types =
    debugTupleArrayFilter typeSelector types

let debugFilterProvider providers =
    debugTupleArrayFilter providerSelector providers