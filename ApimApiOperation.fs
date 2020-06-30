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
        DisplayName: string option
        OperationId: string option
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
        DisplayName = None
        OperationId = None
    })

    member __.Run (cargs, args) =
        let method =
            args.Method |>
            getMethod
            
        let displayName =
            match args.DisplayName with
            | Some dn -> dn
            | None    -> method
        
        let idFrom (displayName : string) =
            displayName.Replace(' ', '-').ToLowerInvariant()
        
        ApiOperationArgs(
            ResourceGroupName = (cargs.Extras |> getResourceGroup |> getName),
            ApiManagementName = getName args.ApiManagement,
            ApiName = getName args.Api,
            UrlTemplate = input args.UrlTemplate,
            Method = input method,
            DisplayName = input displayName,
            OperationId = ((match args.OperationId with | Some x -> x | None -> (idFrom displayName)) |> input)
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
        cargs, { args with DisplayName = Some displayName }
        
    [<CustomOperation("id")>]
    member __.OperationId((cargs, args), id) =
        cargs, { args with OperationId = Some id }

let apiOperation = ApimApiOperationBuilder()