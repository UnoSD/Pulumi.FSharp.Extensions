#r "paket:
nuget FSharp.Core 5.0.0.0
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.BuildServer.TeamFoundation
nuget Fake.Core.Xml
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.IO.Globbing.Operators
open Fake.DotNet.NuGet
open Fake.BuildServer
open Fake.Core.Xml
open Fake.DotNet
open Fake.Core
open System

// Local usage:
// dotnet fake run build.fsx -- <provider>

BuildServer.install [ TeamFoundation.Installer ]

let provider =
    match Environment.environVarOrNone "PROVIDER", lazy(Context.forceFakeContext().Arguments) with
    | Some p, _
    | None  , Lazy([ p ]) -> p
    | _                   -> failwith "Missing provider"

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

let nextExtensionsVersion =
    NuGet.getLatestPackage (NuGet.getRepoUrl()) fullName |>
    (fun x -> x.Version.Split('.')) |>
    Array.last |>
    Int32.Parse |>
    (+)1

let packOptions options : DotNet.PackOptions = {
    options with
        MSBuildParams = { 
            options.MSBuildParams with
                Properties = 
                    ("PackageVersion", sprintf "%s.%i" pulumiNuGetVersion nextExtensionsVersion) :: 
                    options.MSBuildParams.Properties
        }
}

let pushOptions options : DotNet.NuGetPushOptions = {
    options with
        PushParams = {
            options.PushParams with
                ApiKey = Environment.environVarOrFail "NUGETAPIKEY" |> Some
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