module Schema

open System
open FSharp.Data
open System.IO
open Fantomas

let private getSchemaFromFileCacheOrDownload provider version schemaUrl =
    match FileInfo($"{provider}.{version}.json") with
    | fi when fi.Exists -> fi.OpenText().ReadToEnd()
    | fi                -> let json = Http.RequestString(schemaUrl)
                           #if DEBUG
                           json |> fi.CreateText().Write
                           #endif
                           json
    
let private loadSchema' version provider =
    $"https://raw.githubusercontent.com/pulumi/pulumi-{provider}/v{version}" +
    $"/provider/cmd/pulumi-resource-{provider}/schema.json" |>
    getSchemaFromFileCacheOrDownload provider version |>
    JsonValue.Parse
    
let loadSchema provider version =
    String.map Char.ToLower provider |>
    loadSchema' version