// Repetition is going beyond ridicolous,
// create a type provider or, at least, a
// code text generator

[<AutoOpen>]
module Pulumi.FSharp.Azure.Legacy.FunctionApp

open Pulumi.Azure.AppService.Inputs
open Pulumi.FSharp.Azure.Common
open Pulumi.Azure.AppInsights
open Pulumi.Azure.AppService
open Pulumi.Azure.Storage
open Pulumi.FSharp
open Pulumi
    
type Runtime =
    | Dotnet

type AppSetting =
    | Runtime of Runtime
    | Package of Output<string>
    | AppInsight of Insights
    | Custom of string * string
    | CustomIO of string * Output<string>

module Internal =    
    type FunctionAppArgsRecord = {
        StorageAccount: Account
        Plan: Plan
        AppSettings: AppSetting list
        Version: string
        AllowedOrigin: Output<string>
        CorsCredentials: bool
    }
        
    let getAppSetting =
        function
        | Runtime runtime       -> "runtime", input (runtime.ToString().ToLower())
        | Package package       -> "WEBSITE_RUN_FROM_PACKAGE", io package
        | AppInsight ai         -> "APPINSIGHTS_INSTRUMENTATIONKEY", io ai.InstrumentationKey
        | Custom (key, value)   -> key, input value
        | CustomIO (key, value) -> key, io value

open Internal

type FunctionAppBuilder internal () =
    inherit AzureResource()

    member __.Yield _ = (AzureResource.Zero, {
        StorageAccount = null
        Plan = null
        AppSettings = []
        Version = "~3"
        AllowedOrigin = Output.Create("")
        CorsCredentials = true
    })

    member __.Run (cargs, args) =
        let functionAppCors =
            input (FunctionAppSiteConfigCorsArgs(AllowedOrigins = inputList [ io args.AllowedOrigin ],
                                                 SupportCredentials = input args.CorsCredentials))
    
        let appSettings =
            args.AppSettings
            |> List.map getAppSetting
        
        FunctionAppArgs(
            ResourceGroupName = (cargs.Extras |> getResourceGroup |> getName),
            AppServicePlanId = io args.Plan.Id,
            AppSettings = inputMap appSettings,
            StorageAccountName = io args.StorageAccount.Name,
            StorageAccountAccessKey = io args.StorageAccount.PrimaryAccessKey,
            Version = input args.Version,
            SiteConfig = input (FunctionAppSiteConfigArgs(Cors = functionAppCors))
        )
        |> fun faa -> FunctionApp(cargs.Name, faa)
    
    [<CustomOperation("runtime")>]
    member __.Runtime((cargs, args), runtime) =
        cargs, { args with AppSettings = AppSetting.Runtime runtime :: args.AppSettings }
    
    [<CustomOperation("plan")>]
    member __.Plan((cargs, args), plan) =
        cargs, { args with Plan = plan }
    
    [<CustomOperation("storageAccount")>]
    member __.StorageAccount((cargs, args), storageAccount) =
        cargs, { args with StorageAccount = storageAccount }
        
    [<CustomOperation("appSettings")>]
    member __.AppSettings((cargs, args), appSettings) =
        cargs, { args with AppSettings = appSettings }
        
    [<CustomOperation("version")>]
    member __.Version((cargs, args), version) =
        cargs, { args with Version = version }
        
    [<CustomOperation("allowedOrigin")>]
    member __.AllowedOrigin((cargs, args), allowedOrigin) =
        cargs, { args with AllowedOrigin = allowedOrigin }
        
    [<CustomOperation("corsCredentials")>]
    member __.CorsCredentials((cargs, args), corsCredentials) =
        cargs, { args with CorsCredentials = corsCredentials }

/// <summary>WARNING: The Function App builder has been hacked together quickly and will likely introduce breaking changes soon.</summary>
let functionApp = FunctionAppBuilder()