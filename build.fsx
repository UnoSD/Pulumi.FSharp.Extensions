#r "paket:
nuget FSharp.Core 4.7.2
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.BuildServer.TeamFoundation
nuget Fake.Core.CommandLineParsing
nuget Newtonsoft.Json
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
open Fake.IO
open System

(*
    Usage:

    dotnet fake run build.fsx AzureAD                      # Install, ForceRegeneration, Pack for AzureAD
    dotnet fake run build.fsx AzureAD -t ForceRegeneration # Install and ForceRegeneration only
    dotnet fake run build.fsx All     -t ForceRegeneration # Install and ForceRegeneration for all providers
    dotnet fake run build.fsx All     -t Build             # Install, ForceRegeneration and Build for all providers
*)

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
    | Some "All" -> "*"
    | Some p     -> p
    | _          -> failwith "Missing provider"

let getProviders =
    function
    | "*"   -> DirectoryInfo.ofPath "." |>
               DirectoryInfo.getSubDirectories |>
               Array.filter (fun d -> d.Name.StartsWith("Pulumi.FSharp.") &&
                                      not <| d.Name.EndsWith(".Test") &&
                                      not <| d.Name.EndsWith(".Core") &&
                                      not <| d.Name.EndsWith(".Myriad")) |>
               Array.map (fun d -> d.Name.Substring(14))
    | name  -> [| name |]

let getFullName provider =
    sprintf "Pulumi.FSharp.%s" provider

let getProjectFiles provider = 
    let projectPattern =
        provider |> getFullName |> sprintf "**/%s.fsproj" 
        
    !! projectPattern

let traceNested func projFile =
    Trace.useWith true
                  (fun _ -> func projFile)
                  (Trace.traceTarget projFile "" "")
                  // traceTask, traceTag

let vaultFile =
    "Pulumi.FSharp.Extensions.vault.json"

let nuGetApiKey = 
    "nuGetApiKey"

let confirm msg =
    printf "%s [y/any]? " msg
    Console.ReadKey().Key = ConsoleKey.Y

Target.create "CreateVault" (fun _ ->
    if not <| File.exists vaultFile ||
       confirm "Vault file exists, do you want to overwrite it" then

        printfn "\nEnter the NuGet API key: "

        let variable : Vault.Variable = {
            Name = nuGetApiKey
            Secret = false // On Azure DevOps SecureFile is already encrypted
            Value = Console.ReadLine()
        }

        let vaultFileContent = 
            //let keyInfo = Some "keyFilePath" |> Vault.createKey
            {| 
                // Only if encrypted
                //keyFile = keyInfo.KeyFile
                //iv = keyInfo.Iv
                values = [| variable |]
            |} |> 
            Newtonsoft.Json.JsonConvert.SerializeObject
        
        File.WriteAllText(vaultFile, vaultFileContent)
)

Target.create "Install" (fun _ ->
    DotNet.Options.Create() |>
    DotNet.install DotNet.Versions.FromGlobalJson |>
    ignore
)

Target.create "ForceRegeneration" (fun _ ->
    let myriadFiles =
        getProvider args |>
        getProviders |>
        Seq.map (fun provider -> provider, 
                                 (!! (provider |>
                                      getFullName |>
                                      sprintf "**/%s/Myriad.fs"))
                                 |> Seq.exactlyOne) |>
        Map.ofSeq

    let random =
        DateTime.Now.Millisecond |>
        Random
    
    let moduleDeclaration provider =
        sprintf "module private %s" provider

    let forceRebuild =
        match BuildServer.isLocalBuild with
        | true  -> random.Next() |> sprintf "module Force = let private nonce = %i"
        | false -> ""
    
    myriadFiles |>
    Map.iter (fun provider myriadFile -> 
                File.WriteAllText(myriadFile, 
                                  sprintf "%s\n\n%s" (moduleDeclaration provider) forceRebuild))
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
    getProviders |>
    Array.map (getProjectFiles >> Seq.exactlyOne) |>
    Seq.iter (traceNested (DotNet.build buildOptions))
)

Target.create "PublishGeneratedCode" (fun _ ->
    !! (getProvider args |> getFullName |> sprintf "**/%s/Generated.fs") |>
    Seq.iter (Trace.publish ImportData.BuildArtifact)
)

type NuGetVersions =
    {
        versions: string[]
    }

Target.create "Pack" (fun _ ->
    getProvider args |>
    getProviders |>
    Seq.iter (fun provider ->
        let nextExtensionsVersion =
            getFullName provider |> 
            NuGet.getLatestPackage (NuGet.getRepoUrl()) |>
            (fun x -> x.Version.Split('.')) |>
            Array.last |>
            Int32.Parse |>
            (+)4
            
        let xPath =
            sprintf "/Project/ItemGroup/PackageReference[@Include='Pulumi.%s']" provider
            
        let projectFile =
            getProjectFiles provider |>
            Seq.exactlyOne

        let pulumiNuGetVersion =
            projectFile |> 
            loadDoc |>
            selectXPathAttributeValue xPath
                                      "Version"
                                      []
        
        let packOptions options : DotNet.PackOptions = {
            options with
                MSBuildParams = { 
                    options.MSBuildParams with
                        DisableInternalBinLog = true
                        Properties = 
                            ("PackageVersion", sprintf "%s.%i" pulumiNuGetVersion nextExtensionsVersion) ::
                            ("NoRegenerate","true") ::
                            options.MSBuildParams.Properties
                }
        }
        
        projectFile |>
        traceNested (DotNet.pack packOptions)
    )
)

Target.create "Push" (fun _ ->
    let vaultFile =
        Environment.environVarOrNone "FAKEVAULTFILE_SECUREFILEPATH" |>
        Option.defaultValue vaultFile |>
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
                    ApiKey = Vault.tryGet nuGetApiKey vault
                    Source = Some "https://api.nuget.org/v3/index.json"
            }
    }
    
    !! (getProvider args |> getFullName |> sprintf "**/%s.*.nupkg") |>
    Seq.iter (DotNet.nugetPush pushOptions)
)

Target.create "Default" ignore

"Install"                                              =?>
("ForceRegeneration"   ,     BuildServer.isLocalBuild) ==>
"Build"

"Install"                                              =?>
("ForceRegeneration"   ,     BuildServer.isLocalBuild) ==>
"Pack"                                                 =?>
("PublishGeneratedCode", not BuildServer.isLocalBuild) =?>
("Push"                , not BuildServer.isLocalBuild) ==>
"Default"

args |>
getTarget |>
Target.runOrDefaultWithArguments
