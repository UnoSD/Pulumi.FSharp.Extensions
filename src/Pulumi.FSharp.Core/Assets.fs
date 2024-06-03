module Pulumi.FSharp.Assets

open System.Collections.Generic
open Pulumi

type FileArchive = {
    ArchivePath: string
} with

    member this.ToPulumiType = FileArchive(this.ArchivePath)

type RemoteArchive = {
    ArchiveUri: string
} with

    member this.ToPulumiType = RemoteArchive(this.ArchiveUri)

type RemoteAsset = {
    Uri: string
} with

    member this.ToPulumiType = RemoteAsset(this.Uri)

type FileAsset = {
    Path: string
} with

    member this.ToPulumiType = FileAsset(this.Path)

type StringAsset = {
    Text: string
} with

    member this.ToPulumiType = StringAsset(this.Text)

type Any = { Value: obj }

type Asset =
    | File of FileAsset
    | FileArchive of FileArchive
    | Remote of RemoteAsset
    | RemoteArchive of RemoteArchive
    | String of StringAsset

type AssetArchive = {
    Assets: Map<string, Asset>
} with

    member this.ToPulumiType =
        this.Assets
        |> Map.map (fun _ a ->
            match a with
            | File f -> f.ToPulumiType :> AssetOrArchive
            | FileArchive f -> f.ToPulumiType :> AssetOrArchive
            | Remote f -> f.ToPulumiType :> AssetOrArchive
            | RemoteArchive f -> f.ToPulumiType :> AssetOrArchive
            | String f -> f.ToPulumiType :> AssetOrArchive
        )
        |> Dictionary<string, AssetOrArchive>
        |> AssetArchive
