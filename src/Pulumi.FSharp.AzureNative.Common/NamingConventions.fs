module Pulumi.FSharp.NamingConventions.Azure

open AzureLocations
open Pulumi

// https://docs.microsoft.com/en-us/azure/cloud-adoption-framework/ready/azure-best-practices/resource-naming

module Region =
    let private shorten (regionName: string) =
        regionName
            .Replace("europe", "eu")
            .Replace("brazil", "br")
            .Replace("australia", "au")
            .Replace("switzerland", "sw")
            .Replace("pacific", "pac")
            .Replace("unitedstates", "us")
            .Replace("central", "c")
            .Replace("north", "n")
            .Replace("south", "s")
            .Replace("east", "e")
            .Replace("west", "w")

    let private configuredRegion = Config("azure-native").Get("location")

    let shortName =
        locationsMap
        |> Map.tryFind configuredRegion
        |> Option.map (fun x ->
            x
            |> shorten
        )
        |> Option.defaultWith (fun () ->
            failwith $"Missing or incorrect azure-native:location: {configuredRegion}"
        )

module Resource =
    let private woa =
        match Config().Get("workloadOrApplication") with
        | null
        | "" -> ""
        | value -> $"{value}-"

    let name resourceType (instanceNumber: int) =
        $"{resourceType}-{woa}{Deployment.Instance.StackName}-{Region.shortName}-{instanceNumber:D3}"

    let nameOne resourceType = name resourceType 1
