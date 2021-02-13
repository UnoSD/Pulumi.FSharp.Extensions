module Pulumi.FSharp.Myriad

open System.IO
open System.Xml
open AstConfiguration
open Myriad.Core
open AstHelpers
open AstModules

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
            
            let url =
                getSchemaUrl provider version
            
            [Namespace.namespace'($"Pulumi.FSharp.{provider}", [
                yield  Module.open'("Pulumi.FSharp")
                
                yield! createPulumiModules url provider version
            ])]

        member this.ValidInputExtensions = seq { ".fs" }