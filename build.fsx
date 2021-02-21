#r "paket:
nuget FSharp.Core 5.0.0.0
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.BuildServer.TeamFoundation
nuget Fake.Core.Xml
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.DotNet.NuGet
open Fake.BuildServer
open Fake.Core.Xml
open Fake.DotNet
open Fake.Core
open System.IO
open System

// Local usage:
// dotnet fake run build.fsx -- <provider>

BuildServer.install [ TeamFoundation.Installer ]

let vaultFile =
    Environment.environVarOrNone "FAKEVAULTFILE_SECUREFILEPATH" |>
    Option.defaultValue "Pulumi.FSharp.Extensions.vault.json" |>
    FileInfo

let vault =
    match Vault.fromFakeEnvironmentOrNone(), vaultFile.Exists with
    | Some vault, _     -> vault
    | None      , true  -> vaultFile.OpenText().ReadToEnd() |> Vault.fromJson
    | None      , false -> failwith "Unsupported source for secrets"

let provider =
    // Environment.environVarOrNone "BUILD_DEFINITIONNAME"
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
                ApiKey = Vault.tryGet "nuGetApiKey" vault
                Source = Some "https://api.nuget.org/v3/index.json"
        }
}

Target.create "Install" (fun _ ->
    DotNet.install DotNet.Versions.FromGlobalJson (DotNet.Options.Create()) |> ignore
)

Target.create "Build" (fun _ ->
    DotNet.build buildOptions projectFile
)

Target.create "PublishGeneratedCode" (fun _ ->
    !! (sprintf "**/%s/Generated.fs" fullName) |>
    Seq.exactlyOne |>
    Trace.publish ImportData.BuildArtifact
)

Target.create "Pack" (fun _ ->
    DotNet.pack packOptions projectFile
)

Target.create "Push" (fun _ ->
    !! (sprintf "**/%s.*.nupkg" fullName) |>
    Seq.exactlyOne |>
    DotNet.nugetPush pushOptions
)

Target.create "Default" ignore

"Install"                                              ==>
"Build"                                                =?>
("PublishGeneratedCode", not BuildServer.isLocalBuild) ==>
"Pack"                                                 =?>
("Push"                , not BuildServer.isLocalBuild) ==>
"Default"

Target.runOrDefaultWithArguments "Default"