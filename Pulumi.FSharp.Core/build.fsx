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
open Fake.BuildServer
open Fake.DotNet
open Fake.Core
open System.IO

BuildServer.install [ TeamFoundation.Installer ]

let args =
    Context.forceFakeContext().Arguments |>
    Array.ofList |>
    Docopt("""
usage: dotnet_fake_run_build.fsx [options]

options:
 -t <target>
    """).Parse

let getTarget args =
    match Map.tryFind "-t" args with
    | Some (Argument t) -> t
    | _                 -> "Default"

let getProjectFile () = 
    !! "**/Pulumi.FSharp.Core.fsproj" |> Seq.head

Target.create "Install" (fun _ ->
    DotNet.Options.Create() |>
    DotNet.install DotNet.Versions.FromGlobalJson |>
    ignore
)

Target.create "Build" (fun _ ->
    let buildOptions options : DotNet.BuildOptions = {
            options with 
                Common = {
                    options.Common with
                        Verbosity = Some DotNet.Verbosity.Quiet
                }
                NoLogo = true
        }
    
    getProjectFile () |>
    DotNet.build buildOptions
)

Target.create "Pack" (fun _ ->
    getProjectFile () |>
    DotNet.pack id
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
    
    !! "**/Pulumi.FSharp.Core.*.nupkg" |>
    Seq.exactlyOne |>
    DotNet.nugetPush pushOptions
)

Target.create "Default" ignore

"Install"                                              ==>
"Pack"                                                 =?>
("Push"                , not BuildServer.isLocalBuild) ==>
"Default"

args |>
getTarget |>
Target.runOrDefaultWithArguments