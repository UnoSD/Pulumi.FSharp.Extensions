module Pulumi.FSharp.Myriad

open AstNamespace
open AstModules
open Myriad.Core
open System.Xml
open System.IO

[<MyriadGenerator("Pulumi.FSharp")>]
type PulumiFSharpGenerator() =
    interface IMyriadGenerator with
        member _.Generate(context) =            
            let projectFile =
                FileInfo(context.InputFileName).Directory.EnumerateFiles("*.fsproj") |>
                Seq.exactlyOne

            let provider =
                projectFile.Name.["Pulumi.FSharp.".Length..^".fsproj".Length]

            let fsproj =
                XmlDocument() in fsproj.Load (projectFile.OpenText())
                
            let version = 
                fsproj.SelectNodes("/Project/ItemGroup") |>
                Seq.cast<XmlNode> |>
                Seq.collect (fun y -> y.SelectNodes("PackageReference") |> Seq.cast<XmlNode>) |>
                Seq.pick (fun x -> if x.Attributes.["Include"].Value = $"Pulumi.{provider}" then
                                       x.Attributes.["Version"].Value |> Some
                                   else
                                       None)

            $"https://raw.githubusercontent.com/pulumi/pulumi-{provider.ToLower()}/v{version}" +
            $"/provider/cmd/pulumi-resource-{provider.ToLower()}/schema.json" |>
            downloadSchema provider version |>
            createPulumiModules |>
            namespace' provider |>
            List.singleton

        member this.ValidInputExtensions = seq { ".fs" }