module Pulumi.FSharp.Myriad

open AstNamespace
open IndexModule
open Myriad.Core
open AstModules
open System.Xml
open System.IO
open Schema
open Paket

[<MyriadGenerator("Pulumi.FSharp")>]
type PulumiFSharpGenerator() =

    interface IMyriadGenerator with
        member this.Generate(context) =

            let getVersionFromFsproj (projectFile: FileInfo) (provider: string) =
                let fsproj : XmlDocument = XmlDocument()
                in do fsproj.Load (projectFile.OpenText())
        
                fsproj.SelectNodes("/Project/ItemGroup")
                |> Seq.cast<XmlNode> 
                |> Seq.collect (fun y -> 
                    y.SelectNodes("PackageReference") 
                    |> Seq.cast<XmlNode>)
                    |> Seq.pick (fun x -> 
                        if x.Attributes["Include"].Value = $"Pulumi.{provider}" then
                            x.Attributes["Version"].Value |> Some
                        else
                            None)
        
            let projectFile =
                FileInfo(context.InputFilename).Directory.EnumerateFiles("*.fsproj") |>
                Seq.exactlyOne

            let provider =
                let providerName = projectFile.Name["Pulumi.FSharp.".Length..^".fsproj".Length]
                let providerNameOverride =
                    Map.ofList ["AzureNativeV2", "AzureNative"]

                providerNameOverride
                |> Map.tryFind providerName
                |> Option.defaultValue providerName

            let paketDeps = 
                FileInfo(context.InputFilename).DirectoryName
                |> Paket.Dependencies.TryLocate

            let paketProviderVersion =
                paketDeps
                |> Option.bind(fun deps ->
                    deps.GetInstalledVersion $"Pulumi.{provider}")
                |> Option.map (fun version -> 
                    let semver = (SemVer.Parse version) in
                    $"{semver.Major}.{semver.Minor}.{semver.Patch}")
                

            let version = 
                match paketProviderVersion with
                | Some version -> version
                | None -> getVersionFromFsproj projectFile provider

            let providerRepositoryNameOverride =
                ["AzureNative", "azure-native"]
                |> Map.ofList

            let providerRepository =
                providerRepositoryNameOverride
                |> Map.tryFind provider
                |> Option.defaultValue provider

            loadSchema providerRepository version
            |> createTypes
            |> createModules provider
            |> createNamespace
            |> List.singleton
            |> Output.Ast

        member this.ValidInputExtensions = seq { ".fs" }