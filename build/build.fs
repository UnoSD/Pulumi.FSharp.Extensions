#nowarn "57"

open System
open System.IO

open Argu
open Fake.Api
open Fake.BuildServer
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools
open global.Paket.PackageSources


let environVarAsBoolOrDefault varName defaultValue =
    let truthyConsts = [
        "1"
        "Y"
        "YES"
        "T"
        "TRUE"
    ]

    try
        let envvar = (Environment.environVar varName).ToUpper()

        truthyConsts
        |> List.exists ((=) envvar)
    with _ ->
        defaultValue

//-----------------------------------------------------------------------------
// Metadata and Configuration
//-----------------------------------------------------------------------------

let rootDirectory =
    __SOURCE_DIRECTORY__
    </> ".."

let productName = "Pulumi.FSharp.Extensions"

let sln =
    rootDirectory
    </> $"{productName}.sln"

let srcCodeGlob =
    !!(rootDirectory
       </> "src/**/*.fs")
    ++ (rootDirectory
        </> "src/**/*.fsx")
    -- (rootDirectory
        </> "src/**/obj/**/*.fs")

let testsCodeGlob =
    !!(rootDirectory
       </> "tests/**/*.fs")
    ++ (rootDirectory
        </> "tests/**/*.fsx")
    -- (rootDirectory
        </> "tests/**/obj/**/*.fs")

let srcDir =
    rootDirectory
    </> "src"

let srcCoreDir =
    srcDir
    </> "Pulumi.FSharp.Core"
    </> "Pulumi.FSharp.Core.fsproj"

let myriadPluginDir =
    srcDir
    </> "Pulumi.FSharp.Myriad"
    </> "Pulumi.FSharp.Myriad.fsproj"

let providersDir =
    rootDirectory
    </> "providers"

let srcGlob =
    srcDir
    </> "**/*.??proj"

let providersGlob =
    providersDir
    </> "**/*.??proj"

let testsGlob =
    rootDirectory
    </> "tests/**/*.??proj"

let srcAndTest =
    !!srcGlob
    ++ testsGlob

let distDir =
    rootDirectory
    </> "dist"

let distGlob =
    distDir
    </> "*.nupkg"

let coverageThresholdPercent = 80

let coverageReportDir =
    rootDirectory
    </> "docs"
    </> "coverage"


let docsDir =
    rootDirectory
    </> "docs"

let docsSrcDir =
    rootDirectory
    </> "docsSrc"

let temp =
    rootDirectory
    </> "temp"

let watchDocsDir =
    temp
    </> "watch-docs"


let gitOwner = "cagyirey"
let gitRepoName = "Pulumi.FSharp.Extensions"

let gitHubRepoUrl = sprintf "https://github.com/%s/%s/" gitOwner gitRepoName

let documentationRootUrl = sprintf "https://%s.github.io/%s/" gitOwner gitRepoName

let releaseBranch = "main"
let readme = "README.md"
let changelogFile = "CHANGELOG.md"

let READMElink = Uri(Uri(gitHubRepoUrl), $"blob/{releaseBranch}/{readme}")

let publishUrl = $"https://nuget.pkg.github.com/{gitOwner}/index.json"

let enableCodeCoverage = environVarAsBoolOrDefault "ENABLE_COVERAGE" false

let githubToken = Environment.environVarOrNone "GITHUB_TOKEN"

let nugetToken = Environment.environVarOrNone "GITHUB_TOKEN" // "NUGET_TOKEN"

let githubPackageSource =
    NuGetV3 {
        Url = publishUrl
        Authentication =
            match githubToken with
            | Some token ->
                Paket.NetUtils.AuthProvider.ofUserPassword {
                    Username = GitHubActions.Environment.Actor
                    Password = token
                    Type = Paket.NetUtils.AuthType.Basic
                }
            | None -> Paket.AuthService.GetGlobalAuthenticationProvider publishUrl
    }

let githubSHA = Environment.environVarOrNone "GITHUB_SHA"

let shortGitShubHA =
    githubSHA
    |> Option.map (fun sha -> sha.[..7])

//-----------------------------------------------------------------------------
// Helpers
//-----------------------------------------------------------------------------

let isRelease (targets: Target list) =
    targets
    |> Seq.map (fun t -> t.Name)
    |> Seq.exists ((=) "PublishToNuGet")

let invokeAsync f = async { f () }

let configuration (targets: Target list) =
    let defaultVal = if isRelease targets then "Release" else "Debug"

    match Environment.environVarOrDefault "CONFIGURATION" defaultVal with
    | "Debug" -> DotNet.BuildConfiguration.Debug
    | "Release" -> DotNet.BuildConfiguration.Release
    | config -> DotNet.BuildConfiguration.Custom config

let failOnBadExitAndPrint (p: ProcessResult) =
    if
        p.ExitCode
        <> 0
    then
        p.Errors
        |> Seq.iter Trace.traceError

        failwithf "failed with exitcode %d" p.ExitCode


let isCI = lazy environVarAsBoolOrDefault "CI" false

// CI Servers can have bizarre failures that have nothing to do with your code
let rec retryIfInCI times fn =
    match isCI.Value with
    | true ->
        if times > 1 then
            try
                fn ()
            with _ ->
                retryIfInCI (times - 1) fn
        else
            fn ()
    | _ -> fn ()

let failOnWrongBranch () =
    if
        Git.Information.getBranchName ""
        <> releaseBranch
    then
        failwithf "Not on %s.  If you want to release please switch to this branch." releaseBranch


module PulumiExtensions =

    let getProviderName projectFile =
        (FileInfo projectFile).Name["Pulumi.FSharp.".Length .. ^".fsproj".Length]

    let getProviderVersion providerName =
        let dependencies = Paket.Dependencies.Locate()
        let providerNameOverride = Map.ofList [ "AzureNativeV2", "AzureNative" ]

        let provider =
            providerNameOverride
            |> Map.tryFind providerName
            |> Option.defaultValue providerName

        dependencies
            .GetInstalledPackageModel(Some "Providers", $"Pulumi.{provider}")
            .PackageVersion.Normalize()

    let getProviderVersionFromFsproj projectFile =
        getProviderName projectFile
        |> getProviderVersion

    let isProviderPublished provider =
        try
            Paket.Dependencies.FindPackageVersions(
                rootDirectory,
                [ githubPackageSource ],
                $"Pulumi.FSharp.{provider}"
            )
        with e ->
            Array.empty
        |> Set.ofArray
        |> Set.map (SemVer.parse)
        |> Set.contains (
            getProviderVersion provider
            |> SemVer.parse
        )


    let getProviderVersions (lock1: Paket.LockFile) (lock2: Paket.LockFile) =
        (lock1.Groups[Paket.Domain.GroupName "Providers"].Resolution
         |> Map.toList)
        @ (lock2.Groups[Paket.Domain.GroupName "Providers"].Resolution
           |> Map.toList)
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, List.map snd v)
        |> Map.ofList
        |> Map.map (fun package versions ->
            match versions with
            | [ v1; v2 ] ->
                (min v1.Version v2.Version,
                 Some
                 <| max v1.Version v2.Version)
            | [ version ] -> (version.Version, None)
        )

module NuGet =

    let isPublished project =
        let projectFile = FileInfo project

        let changelog: Changelog.Changelog =
            projectFile.DirectoryName
            </> "CHANGELOG.md"
            |> Changelog.load

        Paket.Dependencies.FindPackageVersions(
            rootDirectory,
            [ githubPackageSource ],
            System.IO.Path.GetFileNameWithoutExtension project
        )
        |> Set.ofArray
        |> Set.map (SemVer.parse)
        |> Set.contains changelog.LatestEntry.SemVer
        |> not

    let shouldPublishCore = Lazy<_>.Create(fun () -> isPublished srcCoreDir)

    let shouldPublishMyriadPlugin =
        Lazy<_>.Create(fun () -> isPublished myriadPluginDir)

module dotnet =
    let watch cmdParam program args =
        DotNet.exec cmdParam (sprintf "watch %s" program) args

    let run cmdParam args = DotNet.exec cmdParam "run" args

    let tool optionConfig command args =
        DotNet.exec optionConfig (sprintf "%s" command) args
        |> failOnBadExitAndPrint

    let reportgenerator optionConfig args =
        tool optionConfig "reportgenerator" args

    let sourcelink optionConfig args = tool optionConfig "sourcelink" args

    let fcswatch optionConfig args = tool optionConfig "fcswatch" args

    let fsharpAnalyzer optionConfig args =
        tool optionConfig "fsharp-analyzers" args

    let fantomas args = DotNet.exec id "fantomas" args

module FSharpAnalyzers =
    type Arguments =
        | Project of string
        | Analyzers_Path of string
        | Fail_On_Warnings of string list
        | Ignore_Files of string list
        | Verbose

        interface IArgParserTemplate with
            member s.Usage = ""

let failOnLocalBuild () =
    if not isCI.Value then
        failwith "Not on CI. If you want to publish, please use CI."

let failOnCIBuild () =
    if isCI.Value then
        failwith "On CI. If you want to run this target, please use a local build."

let allPublishChecks () = failOnLocalBuild ()

//-----------------------------------------------------------------------------
// Target Implementations
//-----------------------------------------------------------------------------

/// So we don't require always being on the latest MSBuild.StructuredLogger
let disableBinLog (p: MSBuild.CliArguments) = { p with DisableInternalBinLog = true }

let clean _ =
    [
        "bin"
        "temp"
        distDir
        coverageReportDir
    ]
    |> Shell.cleanDirs

    !!srcGlob
    ++ testsGlob
    |> Seq.collect (fun p ->
        [
            "bin"
            "obj"
        ]
        |> Seq.map (fun sp ->
            IO.Path.GetDirectoryName p
            </> sp
        )
    )
    |> Shell.cleanDirs

    [ "paket-files/paket.restore.cached" ]
    |> Seq.iter Shell.rm

let dotnetRestore _ =
    [ sln ]
    |> Seq.map (fun dir ->
        fun () ->
            let args =
                []
                |> String.concat " "

            DotNet.restore
                (fun c -> {
                    c with
                        MSBuildParams = disableBinLog c.MSBuildParams
                        Common =
                            c.Common
                            |> DotNet.Options.withCustomParams (Some(args))
                })
                dir
    )
    |> Seq.iter (retryIfInCI 10)

let buildProvider projectFile =
    fun (ctx: TargetParameter) ->
        let args = [
            $"/p:VersionPrefix={PulumiExtensions.getProviderVersionFromFsproj projectFile}"
            //"/p:NoRegenerate=true"
            "--no-restore"
        ]

        DotNet.build
            (fun c -> {
                c with
                    MSBuildParams = disableBinLog c.MSBuildParams
                    Configuration = configuration (ctx.Context.AllExecutingTargets)
                    Common =
                        c.Common
                        |> DotNet.Options.withAdditionalArgs args

            })
            projectFile


let dotnetBuild project =
    fun ctx ->
        let changelog =
            (FileInfo project).DirectoryName
            </> "CHANGELOG.md"
            |> Changelog.load

        let latestEntry = changelog.LatestEntry

        // Get release notes with properly-linked version number
        let releaseNotes = Changelog.mkReleaseNotes changelog latestEntry gitHubRepoUrl

        let args = [
            sprintf "/p:VersionPrefix=%s" latestEntry.NuGetVersion
            //"/p:NoRegenerate=true"
            "--no-restore"
        ]

        DotNet.build
            (fun c -> {
                c with
                    MSBuildParams = disableBinLog c.MSBuildParams
                    Configuration = configuration (ctx.Context.AllExecutingTargets)
                    Common =
                        c.Common
                        |> DotNet.Options.withAdditionalArgs args

            })
            project

let fsharpAnalyzers _ =
    let argParser =
        ArgumentParser.Create<FSharpAnalyzers.Arguments>(programName = "fsharp-analyzers")

    !!srcGlob
    |> Seq.iter (fun proj ->
        let args =
            [
                FSharpAnalyzers.Analyzers_Path(
                    rootDirectory
                    </> "packages/analyzers"
                )
                FSharpAnalyzers.Arguments.Project proj
                FSharpAnalyzers.Arguments.Fail_On_Warnings [ "BDH0002" ]
                FSharpAnalyzers.Arguments.Ignore_Files [ "*AssemblyInfo.fs" ]
                FSharpAnalyzers.Verbose
            ]
            |> argParser.PrintCommandLineArgumentsFlat

        dotnet.fsharpAnalyzer id args
    )

let dotnetTest ctx =
    let excludeCoverage =
        !!testsGlob
        |> Seq.map IO.Path.GetFileNameWithoutExtension
        |> String.concat "|"

    let isGenerateCoverageReport =
        ctx.Context.TryFindTarget("GenerateCoverageReport").IsSome

    let args = [
        "--no-build"
        if
            enableCodeCoverage
            || isGenerateCoverageReport
        then
            sprintf "/p:AltCover=true"

            if not isGenerateCoverageReport then
                sprintf "/p:AltCoverThreshold=%d" coverageThresholdPercent

            sprintf "/p:AltCoverAssemblyExcludeFilter=%s" excludeCoverage
            "/p:AltCoverLocalSource=true"
    ]

    DotNet.test
        (fun c -> {
            c with
                MSBuildParams = disableBinLog c.MSBuildParams
                Configuration = configuration (ctx.Context.AllExecutingTargets)
                Common =
                    c.Common
                    |> DotNet.Options.withAdditionalArgs args
        })
        sln

let generateCoverageReport _ =
    let coverageReports =
        !! "tests/**/coverage*.xml"
        |> String.concat ";"

    let sourceDirs =
        !!srcGlob
        |> Seq.map Path.getDirectory
        |> String.concat ";"

    let independentArgs = [
        sprintf "-reports:\"%s\"" coverageReports
        sprintf "-targetdir:\"%s\"" coverageReportDir
        // Add source dir
        sprintf "-sourcedirs:\"%s\"" sourceDirs
        // Ignore Tests and if AltCover.Recorder.g sneaks in
        sprintf "-assemblyfilters:\"%s\"" "-*.Tests;-AltCover.Recorder.g"
        sprintf "-Reporttypes:%s" "Html"
    ]

    let args =
        independentArgs
        |> String.concat " "

    dotnet.reportgenerator id args

let showCoverageReport _ =
    failOnCIBuild ()

    coverageReportDir
    </> "index.html"
    |> Command.ShellCommand
    |> CreateProcess.fromCommand
    |> Proc.start
    |> ignore


let watchTests _ =
    !!testsGlob
    |> Seq.map (fun proj ->
        fun () ->
            dotnet.watch
                (fun opt ->
                    opt
                    |> DotNet.Options.withWorkingDirectory (IO.Path.GetDirectoryName proj)
                )
                "test"
                ""
            |> ignore
    )
    |> Seq.iter (
        invokeAsync
        >> Async.Catch
        >> Async.Ignore
        >> Async.Start
    )

    printfn "Press Ctrl+C (or Ctrl+Break) to stop..."

    let cancelEvent =
        Console.CancelKeyPress
        |> Async.AwaitEvent
        |> Async.RunSynchronously

    cancelEvent.Cancel <- true

let packProvider projectFile =
    fun (ctx: TargetParameter) ->
        let args = [
            $"/p:VersionPrefix={PulumiExtensions.getProviderVersionFromFsproj projectFile}"
        ]

        DotNet.pack
            (fun (c: DotNet.PackOptions) -> {
                c.WithCommon(DotNet.Options.withAdditionalArgs args) with
                    NoBuild = true
                    MSBuildParams = disableBinLog c.MSBuildParams
                    Configuration = configuration (ctx.Context.AllExecutingTargets)
                    OutputPath = Some distDir
                    VersionSuffix =
                        // only use a version suffix on non-primary branches
                        Option.filter
                            (fun _ ->
                                Git.Information.getBranchName ""
                                <> releaseBranch
                            )
                            shortGitShubHA

            })
            projectFile

let dotnetPack project =

    fun ctx ->
        let changelog =
            (FileInfo project).DirectoryName
            </> "CHANGELOG.md"
            |> Changelog.load

        let latestEntry = changelog.LatestEntry

        // Get release notes with properly-linked version number
        let releaseNotes = Changelog.mkReleaseNotes changelog latestEntry gitHubRepoUrl

        let args = [
            $"/p:VersionPrefix={latestEntry.NuGetVersion}"
            $"/p:PackageReleaseNotes=\"{releaseNotes}\""
        ]

        DotNet.pack
            (fun c -> {
                c with
                    MSBuildParams = disableBinLog c.MSBuildParams
                    Configuration = configuration (ctx.Context.AllExecutingTargets)
                    OutputPath = Some distDir
                    NoBuild = true
                    VersionSuffix =
                        Option.filter
                            (fun _ ->
                                Git.Information.getBranchName ""
                                <> releaseBranch
                            )
                            shortGitShubHA
                    Common =
                        c.Common
                        |> DotNet.Options.withAdditionalArgs args
            })
            project

let sourceLinkTest _ =
    !!distGlob
    |> Seq.iter (fun nupkg -> dotnet.sourcelink id (sprintf "test %s" nupkg))

let publishProvider packageName =
    fun (_: TargetParameter) ->
        failOnLocalBuild ()
        failOnWrongBranch ()

        let nupkg =
            !!(distDir
               </> $"{packageName}.*.nupkg")
            |> Seq.exactlyOne

        DotNet.nugetPush
            (fun c -> {
                c with
                    PushParams = {
                        c.PushParams with
                            ApiKey =
                                match nugetToken with
                                | Some s -> nugetToken
                                | _ -> c.PushParams.ApiKey // assume paket-config was set properly
                            Source = Some publishUrl
                    }
            })
            nupkg

let publishToNuget _ =
    allPublishChecks ()

    !!distGlob
    |> Seq.iter (
        DotNet.nugetPush (fun c -> {
            c with
                PushParams = {
                    c.PushParams with
                        ApiKey =
                            match nugetToken with
                            | Some s -> nugetToken
                            | _ -> c.PushParams.ApiKey // assume paket-config was set properly
                        Source = Some publishUrl
                }
        })
    )

let paketUpdate _ =
    failOnWrongBranch ()
    failOnLocalBuild ()

    let dependencies = (Paket.Dependencies.Locate()).DependenciesFile

    let oldLockfile =
        (rootDirectory
         </> "paket.lock")
        |> Paket.LockFile.LoadFrom

    if Paket.UpdateProcess.Update(dependencies, Paket.UpdaterOptions.Default) then
        if not (Git.Information.isCleanWorkingCopy rootDirectory) then
            let newLockfile =
                (rootDirectory
                </> "paket.lock")
                |> Paket.LockFile.LoadFrom

            let packageUpdates = PulumiExtensions.getProviderVersions oldLockfile newLockfile

            let newBranch = $"paket-update-{Git.Information.getCurrentHash ()}"
            let prTitle = $"Paket Update for {DateTime.Now}"

            Git.Branches.checkoutNewBranch
                rootDirectory
                (Git.Information.getBranchName rootDirectory)
                newBranch

            Git.Staging.stageFile rootDirectory "paket.lock"
            |> ignore

            Git.Commit.exec rootDirectory prTitle

            Git.Branches.pushBranch rootDirectory gitHubRepoUrl newBranch

            let newPR = Octokit.NewPullRequest(prTitle, newBranch, releaseBranch, Body = prTitle)

            let pr = 
                GitHub.createClientWithToken (Option.get githubToken)
                |> GitHub.createPullRequest gitOwner gitRepoName newPR
                |> Async.RunSynchronously
                |> Async.RunSynchronously

            let prMergeResult = Process.shellExec { 
                Program = "gh"
                WorkingDir = rootDirectory
                CommandLine = $"pr merge {pr.Number} --auto"
                Args = []
            }
            if prMergeResult <> 0 then failwith $"gh pr merge failed with exit code {prMergeResult}"
    else
        failwith "Paket update failed. Unable to create PR."


let formatCode _ =
    let result = dotnet.fantomas $"{srcDir}"

    if not result.OK then
        printfn "Errors while formatting all files: %A" result.Messages

let checkFormatCode ctx =
    let result = dotnet.fantomas $"{srcDir} --check"

    if result.ExitCode = 0 then
        Trace.log "No files need formatting"
    elif result.ExitCode = 99 then
        failwith "Some files need formatting, check output for more info"
    else
        Trace.logf "Errors while formatting: %A" result.Errors

let initTargets () =
    BuildServer.install [ GitHubActions.Installer ]

    /// Defines a dependency - y is dependent on x. Finishes the chain.
    let (==>!) x y =
        x ==> y
        |> ignore

    /// Defines a soft dependency. x must run before y, if it is present, but y does not require x to be run. Finishes the chain.
    let (?=>!) x y =
        x ?=> y
        |> ignore
    //-----------------------------------------------------------------------------
    // Hide Secrets in Logger
    //-----------------------------------------------------------------------------
    Option.iter (TraceSecrets.register "<GITHUB_TOKEN>") githubToken
    Option.iter (TraceSecrets.register "<NUGET_TOKEN>") nugetToken

    //-----------------------------------------------------------------------------
    // Target Declaration
    //-----------------------------------------------------------------------------
    Target.create "Clean" clean
    Target.create "DotnetRestore" dotnetRestore

    Target.create "BuildCore" (dotnetBuild srcCoreDir)
    Target.create "BuildMyriadPlugin" (dotnetBuild myriadPluginDir)
    Target.create "BuildProviders" ignore

    Target.create "DotnetTest" dotnetTest
    Target.create "WatchTests" watchTests

    Target.create "FSharpAnalyzers" fsharpAnalyzers
    Target.create "GenerateCoverageReport" generateCoverageReport
    Target.create "ShowCoverageReport" showCoverageReport

    Target.create "PackCore" (dotnetPack srcCoreDir)
    Target.create "PackMyriadPlugin" (dotnetPack myriadPluginDir)
    Target.create "DotnetPack" ignore
    Target.create "PackProviders" ignore
    Target.create "PublishProviders" ignore
    Target.create "SourceLinkTest" sourceLinkTest
    Target.create "PublishToNuGet" publishToNuget
    Target.create "PaketUpdate" paketUpdate
    Target.create "FormatCode" formatCode
    Target.create "CheckFormatCode" checkFormatCode
    Target.create "Publish" ignore //For CI

    !!providersGlob
    |> Seq.iter (fun projectFile ->
        let providerName = PulumiExtensions.getProviderName projectFile

        Target.create $"BuildProvider.{providerName}" (buildProvider projectFile)
        Target.create $"PackProvider.{providerName}" (packProvider projectFile)

        Target.create
            $"PublishProvider.{providerName}"
            (publishProvider $"Pulumi.FSharp.{providerName}")

        //-----------------------------------------------------------------------------
        // Target Dependencies
        //-----------------------------------------------------------------------------

        "Clean"
        ==>! $"PackProvider.{providerName}"

        "DotnetRestore"
        ==>! $"BuildProvider.{providerName}"

        "BuildMyriadPlugin"
        ==> $"BuildProvider.{providerName}"
        ==> $"PackProvider.{providerName}"
        ==>! $"PublishProvider.{providerName}"

        $"BuildProvider.{providerName}"
        ==>! "BuildProviders"

        $"PackProvider.{providerName}"
        ==>! "PackProviders"

        if not (PulumiExtensions.isProviderPublished providerName) then
            $"PublishProvider.{providerName}"
            ==>! "PublishProviders"
    )

    // Only call Clean if DotnetPack was in the call chain
    // Ensure Clean is called before DotnetRestore
    "Clean"
    ?=>! "DotnetRestore"

    "Clean"
    ==>! "DotnetPack"

    "Clean"
    ==>! "PackCore"

    "Clean"
    ==>! "PackMyriadPlugin"

    "DotnetTest"
    ==> "GenerateCoverageReport"
    ==>! "ShowCoverageReport"

    "DotnetRestore"
    ==>! "WatchTests"


    "DotnetRestore"
    =?> ("CheckFormatCode", isCI.Value)
    ==> "BuildCore"
    ==> "DotnetTest"
    =?> ("PackCore", NuGet.shouldPublishCore.Value)
    ==> "PublishToNuGet"
    ==>! "Publish"

    "DotnetRestore"
    =?> ("CheckFormatCode", isCI.Value)
    ==> "BuildMyriadPlugin"
    ==> "DotnetTest"
    =?> ("PackMyriadPlugin", NuGet.shouldPublishMyriadPlugin.Value)
    ==> "PublishToNuGet"
    ==>! "Publish"

//-----------------------------------------------------------------------------
// Target Start
//-----------------------------------------------------------------------------
[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    initTargets ()
    Target.runOrDefaultWithArguments "Publish"

    0 // return an integer exit code
