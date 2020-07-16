module Debug

open FSharp.Data

let excludeTypes = [
    "azure:core/resourceGroup:ResourceGroup"
    "azure:appservice/plan:Plan"
    "azure:storage/account:Account"
    "azure:storage/container:Container"
    "azure:storage/blob:Blob"
    "azure:appinsights/insights:Insights"
    "azure:core/templateDeployment:TemplateDeployment"
    "azure:apimanagement/api:Api"
    "azure:apimanagement/apiOperation:ApiOperation"
    "azure:appservice/functionApp:FunctionApp"
]

let debugFilters : ((string * JsonValue) [] -> (string * JsonValue) []) =
    // Filtering out the ones that I created manually, for now
    Array.filter (fun (r, _) -> (excludeTypes |> List.contains r |> not)) >>
    // Debug only
    Array.filter (fun (r, _) -> r = "azure:compute/virtualMachine:VirtualMachine")