#r "paket:
nuget FSharp.Core 4.7.0
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.BuildServer.TeamFoundation
nuget Fake.Core.CommandLineParsing
nuget Fake.Core.Xml
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"
#nowarn "52"
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

let getTarget args =
    match Map.tryFind "-t" args with
    | Some (Argument t) -> t
    | _                 -> "Default"

let getProvider args =
    match Map.tryFind "PROVIDER" args |>
          Option.bind (function | Argument p -> Some p | _ -> None) |>
          Option.orElse (Environment.environVarOrNone "PROVIDER") |>
          Option.orElse (Environment.environVarOrNone "BUILD_DEFINITIONNAME") with
    | Some p -> p
    | _      -> failwith "Missing provider"

let getFullName provider =
    sprintf "Pulumi.FSharp.%s" provider

let getProjectFile provider = 
    let projectPattern =
        provider |> getFullName |> sprintf "**/%s.fsproj" 
        
    !! projectPattern |> Seq.head

Target.create "Install" (fun _ ->
    DotNet.install DotNet.Versions.FromGlobalJson (DotNet.Options.Create()) |> ignore
)

Target.create "ForceRegeneration" (fun _ ->
    let myriadFile =
        !! (getProvider args |> getFullName |> sprintf "**/%s/Myriad.fs") |>
        Seq.exactlyOne

    let random =
        DateTime.Now.Millisecond |>
        Random
    
    let moduleDeclaration =
        getProvider args |> sprintf "module private %s"

    let forceRebuild =
        match BuildServer.isLocalBuild with
        | true  -> random.Next() |> sprintf "module Force = let private nonce = %i"
        | false -> ""
    
    File.WriteAllText(myriadFile, sprintf "%s\n\n%s" moduleDeclaration forceRebuild)
)

Target.create "Build" (fun _ ->
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
    
    getProvider args |>
    getProjectFile |>
    DotNet.build buildOptions
)

Target.create "PublishGeneratedCode" (fun _ ->
    !! (getProvider args |> getFullName |> sprintf "**/%s/Generated.fs") |>
    Seq.exactlyOne |>
    Trace.publish ImportData.BuildArtifact
)

Target.create "Pack" (fun _ ->
    let nextExtensionsVersion =
        getProvider args |> 
        getFullName |> 
        NuGet.getLatestPackage (NuGet.getRepoUrl()) |>
        (fun x -> x.Version.Split('.')) |>
        Array.last |>
        Int32.Parse |>
        (+)1
        
    let xPath =
        getProvider args |>
        sprintf "/Project/ItemGroup/PackageReference[@Include='Pulumi.%s']"
        
    let pulumiNuGetVersion =
        getProvider args |>
        getProjectFile |> 
        loadDoc |>
        selectXPathAttributeValue xPath
                                  "Version"
                                  []
    
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
    
    getProvider args |>
    getProjectFile |>
    DotNet.pack packOptions
)

Target.create "Push" (fun _ ->
    let vaultFile =
        Environment.environVarOrNone "FAKEVAULTFILE_SECUREFILEPATH" |>
        Option.defaultValue "Pulumi.FSharp.Extensions.vault.json" |>
        FileInfo

    let vault =
        match Vault.fromFakeEnvironmentOrNone(), vaultFile.Exists with
        | Some vault, _     -> vault
        | None      , true  -> vaultFile.OpenText().ReadToEnd() |> Vault.fromJson
        | None      , false -> failwith "Unsupported source for secrets"

    let pushOptions options : DotNet.NuGetPushOptions = {
        options with
            PushParams = {
                options.PushParams with
                    ApiKey = Vault.tryGet "nuGetApiKey" vault
                    Source = Some "https://api.nuget.org/v3/index.json"
            }
    }
    
    !! (getProvider args |> getFullName |> sprintf "**/%s.*.nupkg") |>
    Seq.exactlyOne |>
    DotNet.nugetPush pushOptions
)

Target.create "Default" ignore

"Install"                                              =?>
("ForceRegeneration"   ,     BuildServer.isLocalBuild) =?>
("PublishGeneratedCode", not BuildServer.isLocalBuild) ==>
"Pack"                                                 =?>
("Push"                , not BuildServer.isLocalBuild) ==>
"Default"

args |>
getTarget |>
Target.runOrDefaultWithArguments