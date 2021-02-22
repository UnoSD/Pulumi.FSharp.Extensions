#r "paket:
nuget FSharp.Core 4.7.0
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.BuildServer.TeamFoundation
nuget Fake.Core.CommandLineParsing
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

BuildServer.install [ TeamFoundation.Installer ]

let args =
    Context.forceFakeContext().Arguments |>
    Array.ofList |>
    Docopt("""
usage: dotnet_fake_run_build.fsx [PROVIDER] [options]

options:
 -t <target>
    """).Parse

let target =
    match Map.tryFind "-t" args with
    | Some (Argument t) -> t
    | _                 -> "Default"

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
    match Map.tryFind "PROVIDER" args |>
          Option.bind (function | Argument p -> Some p | _ -> None) |>
          Option.orElse (Environment.environVarOrNone "PROVIDER") |>
          Option.orElse (Environment.environVarOrNone "BUILD_DEFINITIONNAME") with
    | Some p -> p
    | _      -> failwith "Missing provider"

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
            MSBuildParams = {
                options.MSBuildParams with
                    Properties = ("NoRegenerate","true") :: options.MSBuildParams.Properties
                }
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
                    ("NoRegenerate","true") ::
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

let myriadFile =
    !! (sprintf "**/%s/Myriad.fs" fullName) |>
    Seq.exactlyOne

#nowarn "52"
let random =
    DateTime.Now.Millisecond |>
    Random

Target.create "Install" (fun _ ->
    DotNet.install DotNet.Versions.FromGlobalJson (DotNet.Options.Create()) |> ignore
)

Target.create "ForceRegeneration" (fun _ ->
    let moduleDeclaration =
        sprintf "module private %s" provider

    let forceRebuild =
        match BuildServer.isLocalBuild with
        | true  -> random.Next() |> sprintf "module Force = let private nonce = %i"
        | false -> ""
    
    File.WriteAllText(myriadFile, sprintf "%s\n\n%s" moduleDeclaration forceRebuild)
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

"Install"                                              =?>
("ForceRegeneration"   ,     BuildServer.isLocalBuild) ==>
"Build"                                                =?>
("PublishGeneratedCode", not BuildServer.isLocalBuild) ==>
"Pack"                                                 =?>
("Push"                , not BuildServer.isLocalBuild) ==>
"Default"

Target.runOrDefaultWithArguments target