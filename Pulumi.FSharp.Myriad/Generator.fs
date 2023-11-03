module Pulumi.FSharp.Myriad

open AstNamespace
open IndexModule
open Myriad.Core
open AstModules
open System.Xml
open System.IO
open Schema

[<MyriadGenerator("Pulumi.FSharp")>]
type PulumiFSharpGenerator() =
    interface IMyriadGenerator with
        member _.Generate(context) =
            let projectFile =
                FileInfo(context.InputFilename).Directory.EnumerateFiles("*.fsproj") |>
                Seq.exactlyOne

            let provider =
                projectFile.Name["Pulumi.FSharp.".Length..^".fsproj".Length]

            let providerNameOverride =
                Map.empty
                    .Add("AzureNativeV2", "AzureNative")

            let provider =
                providerNameOverride |>
                Map.tryFind provider |>
                Option.defaultValue provider

            let fsproj =
                XmlDocument() in fsproj.Load (projectFile.OpenText())

            let version =
                fsproj.SelectNodes("/Project/ItemGroup") |>
                Seq.cast<XmlNode> |>
                Seq.collect (fun y -> y.SelectNodes("PackageReference") |> Seq.cast<XmlNode>) |>
                Seq.pick (fun x -> if x.Attributes["Include"].Value = $"Pulumi.{provider}" then
                                       x.Attributes["Version"].Value |> Some
                                   else
                                       None)

            let providerRepositoryNameOverride =
                Map.empty
                   .Add("AzureNative", "azure-native")

            let providerRepository =
                providerRepositoryNameOverride |>
                Map.tryFind provider |>
                Option.defaultValue provider

            loadSchema providerRepository version |>
            createTypes |>
            createModules provider |>
            createNamespace |>
            List.singleton |>
            Output.Ast

        member this.ValidInputExtensions = seq { ".fs" }