[<AutoOpen>]
module Pulumi.FSharp.Azure.Legacy.ArmTemplate

open System.IO
open Pulumi.FSharp.Azure.Common
open Pulumi.Azure.Core
open Pulumi.FSharp
open Pulumi

type Mode =
    | Incremental
    
module ArmTemplateInternal =    
    type ArmTemplateArgsRecord = {
        Json: string
        Parameters: (string * Input<string>) list
        Mode: Mode
    }

open ArmTemplateInternal

type ArmTemplateBuilder internal () =
    inherit AzureResource()
    
    member __.Yield _ = (AzureResource.Zero, {
        Json = ""
        Parameters = []
        Mode = Incremental
    })

    member __.Run (cargs, args) =
        TemplateDeploymentArgs(
            ResourceGroupName = (getName (cargs.Extras |> getResourceGroup)),
            TemplateBody = input args.Json,
            Parameters = inputMap args.Parameters,
            DeploymentMode = input (match args.Mode with | Incremental -> "Incremental"))
        |> fun tda -> TemplateDeployment(cargs.Name, tda)
    
    [<CustomOperation("json")>]
    member __.Json((cargs, args), json) =
        cargs, { args with Json = json }
        
    [<CustomOperation("jsonFile")>]
    member __.JsonFile((cargs, args), path) =
        cargs, { args with Json = File.ReadAllText(path) }        
    
    [<CustomOperation("parameters")>]
    member __.Parameters((cargs, args), parameters) =
        cargs, { args with Parameters = parameters }
    
    [<CustomOperation("mode")>]
    member __.Mode((cargs, args), mode) =
        cargs, { args with Mode = mode }

let armTemplate = ArmTemplateBuilder()