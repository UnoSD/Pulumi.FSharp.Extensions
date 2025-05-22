open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.DotNet.NuGet
open Fake.BuildServer
open Fake.Net.Http
open Fake.Core.Xml
open Fake.DotNet
open Fake.Core
open Fake.IO
open System.IO
open System

(*
    Usage:

    dotnet run --project ./Pulumi.FSharp.Build/Pulumi.FSharp.Build.fsproj -v:m AzureAD                      # Install, ForceRegeneration, Pack for AzureAD
    dotnet run --project ./Pulumi.FSharp.Build/Pulumi.FSharp.Build.fsproj -v:m AzureAD -t ForceRegeneration # Install and ForceRegeneration only
    dotnet run --project ./Pulumi.FSharp.Build/Pulumi.FSharp.Build.fsproj -v:m All     -t ForceRegeneration # Install and ForceRegeneration for all providers
    dotnet run --project ./Pulumi.FSharp.Build/Pulumi.FSharp.Build.fsproj -v:m All     -t Build             # Install, ForceRegeneration and Build for all providers
*)

type NuGetVersions =
    {
        versions: string[]
    }

[<EntryPoint>]
let main args =
    let execContext =
        args |>
        List.ofArray |>
        Context.FakeExecutionContext.Create false "build.fsx"

    Context.RuntimeContext.Fake execContext |>
    Context.setExecutionContext

    BuildServer.install [ TeamFoundation.Installer ]

    let getTarget args =
        match Map.tryFind "-t" args with
        | Some (Argument t) -> t 
        | _                 -> "Default"

    let getProvider args =
        let argsMap =
            args |>
            Array.ofList |>
            Docopt("""
usage: run [PROVIDER] [options]

options:
-t <target>
            """).Parse

        match Map.tryFind "PROVIDER" argsMap |>
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
                
                File.writeString false vaultFile vaultFileContent
        )

    Target.create "Install" (fun _ ->
        DotNet.Options.Create() |>
        DotNet.install DotNet.Versions.FromGlobalJson |>
        ignore
    )

    Target.create "ForceRegeneration" (fun p ->
        let myriadFiles =
            getProvider p.Context.Arguments |>
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
                    File.writeString false myriadFile <| sprintf "%s\n\n%s" (moduleDeclaration provider) forceRebuild))

    Target.create "Build" (fun p ->
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
        
        getProvider p.Context.Arguments |>
        getProviders |>
        Array.map (getProjectFiles >> Seq.exactlyOne) |>
        Seq.iter (traceNested (DotNet.build buildOptions))
    )

    Target.create "PublishGeneratedCode" (fun p ->
        !! (getProvider p.Context.Arguments |> getFullName |> sprintf "**/%s/Generated.fs") |>
        Seq.iter (Trace.publish ImportData.BuildArtifact)
    )

    Target.create "Pack" (fun p ->
        getProvider p.Context.Arguments |>
        getProviders |>
        Seq.iter (fun provider ->
            let nextExtensionsVersion =
                getFullName provider |>
                
                // This does not work and returns wrong package
                //NuGet.getLatestPackage (NuGet.getRepoUrl()) |>
                //(fun x -> x.Version.Split('.')) |>
                
                // So we have to do this crap...
                sprintf "https://www.nuget.org/packages/%s" |>
                get "" "" |>
                (fun x -> x.Split('\n')) |>
                Array.find (fun x -> x.Contains("packageVersion")) |>
                (fun x -> x.Split('"')) |>
                Array.item 1 |>
                (fun x -> x.Split('.')) |>

                Array.last |>
                Int32.Parse |>
                (+)1
                
            let pulumiPackageName =
                Map.empty |>
                Map.add "AzureNativeV2" "AzureNative" |>
                Map.tryFind provider |>
                Option.defaultValue provider
                
            let xPath =
                sprintf "/Project/ItemGroup/PackageReference[@Include='Pulumi.%s']" pulumiPackageName
                
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

    Target.create "Push" (fun p ->
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
        
        !! (getProvider p.Context.Arguments |> getFullName |> sprintf "**/%s.*.nupkg") |>
        Seq.iter (DotNet.nugetPush pushOptions)
    )

    Target.create "Default" ignore

    "Install"                                              =?>
    ("ForceRegeneration"   ,     BuildServer.isLocalBuild) ==>
    "Build" |> ignore

    "Install"                                              =?>
    ("ForceRegeneration"   ,     BuildServer.isLocalBuild) ==>
    "Pack"                                                 =?>
    ("PublishGeneratedCode", not BuildServer.isLocalBuild) =?>
    ("Push"                , not BuildServer.isLocalBuild) ==>
    "Default" |> ignore


    execContext.Arguments |>
        Docopt("""
usage: run [PROVIDER] [options]

options:
-t <target>
    """).Parse |>
    getTarget |>
    Target.runOrDefaultWithArguments
    
    0