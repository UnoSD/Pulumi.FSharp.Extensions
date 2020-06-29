[<AutoOpen>]
module Pulumi.FSharp.Azure.ApimApi

open Pulumi.Azure.ApiManagement
open Pulumi.FSharp.Azure.Core
open Pulumi.FSharp
open Pulumi

type Protocol =
    | Http
    | Https
    | HttpHttps

module ApimApiInternal =    
    type ApimApiArgsRecord = {
        Name: string
        DisplayName: string
        ApiManagement: IOArg<Service>
        Path: string
        Protocol: Protocol
        Revision: string
        ServiceUrl: Output<string>
    }

open ApimApiInternal

type ApimApiBuilder internal () =
    inherit AzureResource()
    
    member __.Yield _ = (AzureResource.Zero, {
        Name = ""
        DisplayName = ""
        ApiManagement = Name ""
        Path = ""
        Protocol = Https
        Revision = "1"
        ServiceUrl = Output.Create("")
    })

    member __.Run (cargs, args) =
        ApiArgs(
            ResourceGroupName = (cargs.Extras |> getResourceGroup |> getName),
            ApiManagementName = getName args.ApiManagement,
            Name = input args.Name,
            DisplayName = input args.DisplayName,
            Path = input args.Path,
            Protocols = ((match args.Protocol with
                          | Http      -> [ "http" ]
                          | Https     -> [ "https" ]
                          | HttpHttps -> [ "https"; "http" ]) |>
                         List.map input |>
                         inputList),
            Revision = input args.Revision,
            ServiceUrl = io args.ServiceUrl
        ) |>
        fun aa -> Api(cargs.Name, aa)
    
    [<CustomOperation("serviceUrl")>]
    member __.ServiceUrl((cargs, args), serviceUrl) =
        cargs, { args with ServiceUrl = serviceUrl }
    
    [<CustomOperation("revision")>]
    member __.Revision((cargs, args), revision) =
        cargs, { args with Revision = revision }        
    
    [<CustomOperation("protocol")>]
    member __.Protocol((cargs, args), protocol) =
        cargs, { args with Protocol = protocol }
    
    [<CustomOperation("apiName")>]
    member __.ApiName((cargs, args : ApimApiArgsRecord), apiName) =
        cargs, { args with Name = apiName }
        
    [<CustomOperation("displayName")>]
    member __.DisplayName((cargs, args : ApimApiArgsRecord), displayName) =
        cargs, { args with DisplayName = displayName }
        
    [<CustomOperation("path")>]
    member __.Path((cargs, args), path) =
        cargs, { args with Path = path }
        
    [<CustomOperation("apim")>]
    member __.ApiManagement((cargs, args), apiManagement) =
        cargs, { args with ApiManagement = Object apiManagement }
    
    member __.ApiManagement((cargs, args), apiManagement) =
        cargs, { args with ApiManagement = Name apiManagement }
    
    member __.ApiManagement((cargs, args), apiManagement) =
        cargs, { args with ApiManagement = IO apiManagement } 

let apimApi = ApimApiBuilder()