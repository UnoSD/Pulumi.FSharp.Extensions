namespace Pulumi.FSharp.Azure

open Pulumi.FSharp.Azure.Core
open Pulumi.Azure.AppInsights
open Pulumi.Azure.Core
open Pulumi.FSharp

module AppInsightPrivate =
    type ApplicationType =
        | IOS
        | Java
        | MobileCenter
        | NodeJs
        | Other
        | Phone
        | Store
        | Web
    
    type AppInsightsArgsRecord = {
        Name: string
        ResourceGroup: IOArg<ResourceGroup>
        ApplicationType: ApplicationType
        RetentionInDays: int
    }
    
    let getType at =
        match at with
        | NodeJs       -> "Node.JS"
        | MobileCenter -> "MobileCenter"
        | other        -> getUnionCaseName(other).ToLowerInvariant()
    
    let run args =
        InsightsArgs(
            ApplicationType = input (getType args.ApplicationType),
            ResourceGroupName = getName args.ResourceGroup,
            RetentionInDays = input args.RetentionInDays
        ) |>
        fun ia -> Insights(args.Name, ia)

[<AutoOpen>]
module AppInsight =
    open AppInsightPrivate

    type AppInsightBuilder internal () =
        member __.Yield _ = {
            Name = ""
            ResourceGroup = Name ""
            ApplicationType = Web
            RetentionInDays = 90
        }

        [<CustomOperation("name")>]
        member __.Name(args : AppInsightsArgsRecord, name) = { args with Name = name }
        
        [<CustomOperation("applicationType")>]
        member __.ApplicationType(args, applicationType) = { args with ApplicationType = applicationType }
        
        [<CustomOperation("retentionInDays")>]
        member __.RetentionInDays(args, days) = { args with RetentionInDays = days }
        
        member __.Run (args : AppInsightsArgsRecord) =
             run args

        [<CustomOperation("resourceGroup")>]
        member __.ResourceGroup(args : AppInsightsArgsRecord, storageAccount) = {
            args with ResourceGroup = Object storageAccount
        }
        
        member __.ResourceGroup(args : AppInsightsArgsRecord, storageAccount) = {
            args with ResourceGroup = Name storageAccount
        }
        
        member __.ResourceGroup(args : AppInsightsArgsRecord, storageAccount) = {
            args with ResourceGroup = IO storageAccount
        } 

    let appInsight = AppInsightBuilder()