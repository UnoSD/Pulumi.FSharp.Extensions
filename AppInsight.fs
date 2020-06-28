[<AutoOpen>]
module Pulumi.FSharp.Azure.AppInsight

open Pulumi.FSharp.Azure.Core
open Pulumi.Azure.AppInsights
open Pulumi.FSharp

type ApplicationType =
    | IOS
    | Java
    | MobileCenter
    | NodeJs
    | Other
    | Phone
    | Store
    | Web

module AppInsightPrivate =
    type AppInsightsArgsRecord = {
        ApplicationType: ApplicationType
        RetentionInDays: int
    }
    
open AppInsightPrivate
    
let private getType at =
    match at with
    | NodeJs       -> "Node.JS"
    | MobileCenter -> "MobileCenter"
    | other        -> getUnionCaseName(other).ToLowerInvariant()

let private run (cargs, args) =
    InsightsArgs(
        ApplicationType = input (getType args.ApplicationType),
        ResourceGroupName = (getResourceGroup cargs.Extras |> getName),
        RetentionInDays = input args.RetentionInDays
    ) |>
    fun ia -> Insights(cargs.Name, ia)

type AppInsightBuilder () =
    inherit AzureResource ()
    
    member __.Yield _ = (AzureResource.Zero, {
        ApplicationType = Web
        RetentionInDays = 90
    })

    [<CustomOperation("applicationType")>]
    member __.ApplicationType((cargs, args), applicationType) =
        cargs, { args with ApplicationType = applicationType }
    
    [<CustomOperation("retentionInDays")>]
    member __.RetentionInDays ((cargs, args), days) =
        cargs, { args with RetentionInDays = days }
    
    member __.Run (cargs, args) =
         run (cargs, args)

let appInsight = AppInsightBuilder()