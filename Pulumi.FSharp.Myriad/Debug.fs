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
            "WindowsVirtualMachineSourceImageReference"
            "WindowsVirtualMachineOsDisk"
            "NetworkInterfaceIpConfiguration"
        ]
        Resources = Include [
            "Blob"
            "WindowsVirtualMachine"
            "NetworkInterface"
            "Subnet"
            "VirtualNetwork"
            "ResourceGroup"
            "Subnet"
            "Account"
            "Container"
        ]
        Providers = Include [
            "compute"
            "storage"
            "network"
            "core"
        ]                        
    }

let private azureNativeFilters = {
        Types     = Include [
            "WindowsVirtualMachineSourceImageReference"
            "WindowsVirtualMachineOsDisk"
            "NetworkInterfaceIpConfiguration"
        ]
        Resources = Include [
            "Blob"
            "WindowsVirtualMachine"
            "NetworkInterface"
            "Subnet"
            "VirtualNetwork"
            "ResourceGroup"
            "Subnet"
            "Account"
            "Container"
        ]
        Providers = Include [
            "compute"
            "storage"
            "network"
            "core"
        ]                        
    }

let private azureAdFilters = {
        Types     = Include [
        ]
        Resources = Include [
            "Application"
            "Group"
        ]
        Providers = Include [
            "index"
        ]                        
    }

let private awsFilters = {
        Types     = Include [
            "BucketWebsite"
        ]
        Resources = Include [
            "Bucket"
        ]
        Providers = Include [
            "s3"
        ]
    }

let private kubernetesFilters = {
        Types     = Include [ 
            "ServiceSpecType"
            "ServiceSpec"
            "DeploymentSpec"
            "LabelSelector"
            "PodTemplateSpec"
            "PodSpec"
            "Container"
            "ContainerPort"
            "ObjectMeta"
        ]
        Resources = Include [
            "Deployment"
        ]
        Providers = Include [ 
            "core/v1"
            "apps/v1"
            "meta/v1" ]
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
let private filters = join [ azureFilters; awsFilters; kubernetesFilters; azureAdFilters; azureNativeFilters ]

let private typeSelector builderType =
    match (builderType, filters.Types, filters.Resources) with
    | (Type     t, Include it, _         )
    | (Resource t, _         , Include it) -> List.contains t.ResourceType.Value it
    | (Type     t, Exclude et, _         )
    | (Resource t, _         , Exclude et) -> not <| List.contains t.ResourceType.Value et

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
