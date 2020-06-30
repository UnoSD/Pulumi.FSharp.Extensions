[<AutoOpen>]
module Pulumi.FSharp.Azure.ApimApiOperation

open Pulumi.Azure.ApiManagement
open Pulumi.FSharp.Azure.Core
open Pulumi.FSharp
    
type HttpMethod =
    | Get
    | Put
    | Post
    | Delete
    | Head
    | Connect
    | Options
    | Trace
    | Patch
    
module ApimApiOperationInternal =    
    type ApimApiOperationArgsRecord = {
        ApiManagement: IOArg<Service>
        Api: IOArg<Api>
        UrlTemplate: string
        Method: HttpMethod
        DisplayName: string
        OperationId: string
    }

open ApimApiOperationInternal

type ApimApiOperationBuilder internal () =
    inherit AzureResource()
    
    let getMethod =
        function
        | Get     -> "GET"
        | Put     -> "PUT"
        | Post    -> "POST"
        | Delete  -> "DELETE"
        | Head    -> "HEAD"
        | Connect -> "CONNECT"
        | Options -> "OPTIONS"
        | Trace   -> "TRACE"
        | Patch   -> "PATCH"
    
    member __.Yield _ = (AzureResource.Zero, {
        ApiManagement = Name ""
        Api = Name ""
        UrlTemplate = "/"
        Method = Get
        DisplayName = ""
        OperationId = ""
    })

    member __.Run (cargs, args) =
        ApiOperationArgs(
            ResourceGroupName = (cargs.Extras |> getResourceGroup |> getName),
            ApiManagementName = getName args.ApiManagement,
            ApiName = getName args.Api,
            UrlTemplate = input args.UrlTemplate,
            Method = (args.Method |> getMethod |> input),
            DisplayName = input args.DisplayName,
            OperationId = input args.OperationId
        ) |>
        fun aoa -> ApiOperation(cargs.Name, aoa)
    
    [<CustomOperation("apim")>]
    member __.Apim((cargs, args), apim) =
        cargs, { args with ApiManagement = Object apim }
        
    member __.Apim((cargs, args), apim) =
        cargs, { args with ApiManagement = IO apim }        
    
    [<CustomOperation("api")>]
    member __.ApiName((cargs, args), api) =
        cargs, { args with Api = Object api }
    
    [<CustomOperation("method")>]
    member __.Method((cargs, args), method) =
        cargs, { args with Method = method }
        
    [<CustomOperation("urlTemplate")>]
    member __.UrlTemplate((cargs, args), urlTemplate) =
        cargs, { args with UrlTemplate = urlTemplate }
        
    [<CustomOperation("displayName")>]
    member __.DisplayName((cargs, args), displayName) =
        cargs, { args with DisplayName = displayName }
        
    [<CustomOperation("id")>]
    member __.OperationId((cargs, args), id) =
        cargs, { args with OperationId = id }

let apiOperation = ApimApiOperationBuilder()