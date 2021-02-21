#r "paket:
nuget FSharp.Core 5.0.0.0
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.BuildServer.TeamFoundation
nuget Fake.Core.Xml
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.IO.Globbing.Operators
open Fake.BuildServer
open Fake.Core.Xml
open Fake.DotNet
open System.IO
open Fake.Core
open System

// Local usage:
// dotnet fake run build.fsx -- <provider>

BuildServer.install [ TeamFoundation.Installer ]

let vaultFile =
    FileInfo("Pulumi.FSharp.Extensions.vault.json")

let vault = 
    match BuildServer.buildServer, vaultFile.Exists with
    | TeamFoundation, _ -> TeamFoundation.variables
    | _, true           -> vaultFile.OpenText().ReadToEnd() |> Vault.fromJson
    | _, false          -> failwith "Unsupported source for secrets"

let provider =
    match BuildServer.buildServer with
    | TeamFoundation -> Environment.environVarOrFail "PROVIDER"
    | LocalBuild     -> Context.forceFakeContext().Arguments |> List.exactlyOne
    | _              -> failwith "Missing provider or unsupported build server"

let fullName =
    sprintf "Pulumi.FSharp.%s" provider

let projectPattern =
    sprintf "**/%s.fsproj" fullName

let projectFile = 
    !! projectPattern |> Seq.head

let xPath =
    sprintf "/Project/ItemGroup/PackageReference[@Include='Pulumi.%s']" provider

let pulumiNuGetVersion =
    projectFile |> 
    loadDoc |>
    selectXPathAttributeValue xPath
                              "Version"
                              []

let buildOptions options : DotNet.BuildOptions = {
        options with 
            Common = {
                options.Common with
                    Verbosity = Some DotNet.Verbosity.Quiet
            }
            NoLogo = true
    }

let extensionsVersion =
    match BuildServer.buildServer, lazy(Int32.TryParse(BuildServer.buildVersion)) with
    | LocalBuild, _                   -> ""
    | _         , Lazy(true, version) -> sprintf ".%i" version
    | _                               -> "Unsupported extension version, if using Azure DevOps try and set " +
                                         "`name: $(Rev:r)` at the top of the pipeline"

let packOptions options : DotNet.PackOptions = {
    options with
        MSBuildParams = { 
            options.MSBuildParams with
                Properties = 
                    ("PackageVersion", sprintf "%s%s" pulumiNuGetVersion extensionsVersion) :: 
                    options.MSBuildParams.Properties
        }
}

let pushOptions options : DotNet.NuGetPushOptions = {
    options with
        PushParams = {
            options.PushParams with
                ApiKey = Vault.tryGet "apiKey" vault
                Source = Some "https://api.nuget.org/v3/index.json"
        }
}

let envDependentAction localBuildAction buildServerAction =
    match BuildServer.isLocalBuild with
    | true  -> localBuildAction
    | false -> buildServerAction

// Deployment tasks

DotNet.install DotNet.Versions.FromGlobalJson

DotNet.build buildOptions projectFile

!! (sprintf "**/%s/Generated.fs" fullName) |>
Seq.exactlyOne |>
Trace.publish ImportData.BuildArtifact

DotNet.pack packOptions projectFile

!! (sprintf "**/%s.*.nupkg" fullName) |>
Seq.exactlyOne |>
envDependentAction ignore (DotNet.nugetPush pushOptions)