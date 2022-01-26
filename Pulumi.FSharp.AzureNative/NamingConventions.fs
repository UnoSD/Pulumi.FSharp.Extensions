module Pulumi.FSharp.NamingConventions.Azure

open FSharp.Data
open Pulumi

// https://docs.microsoft.com/en-us/azure/cloud-adoption-framework/ready/azure-best-practices/resource-naming

module Region = 
    // az account list-locations -o json | sed "s/$(az account show --query id -o tsv)/00000000-0000-0000-0000-000000000000/"
    type private LocationsProvider =
        JsonProvider<LocationsJson.jsonData>
    
    let private locationsJson = LocationsProvider.GetSamples()

    let private shorten (regionName : string) =
        regionName.Replace("europe"      , "eu" )
                  .Replace("brazil"      , "br" )
                  .Replace("australia"   , "au" )
                  .Replace("switzerland" , "sw" )
                  .Replace("pacific"     , "pac")
                  .Replace("unitedstates", "us" )
                  .Replace("central"     , "c"  )
                  .Replace("north"       , "n"  )
                  .Replace("south"       , "s"  )
                  .Replace("east"        , "e"  )
                  .Replace("west"        , "w"  )
    
    let private configuredRegion = Config("azure-native").Require("location").ToLowerInvariant()
    
    let private locationsMap = 
        locationsJson
        |> Array.collect(fun lj -> [| lj.RegionalDisplayName.ToLowerInvariant(), lj
                                      lj.Name.ToLowerInvariant()               , lj
                                      lj.DisplayName.ToLowerInvariant()        , lj |])
        |> Map.ofArray

    let shortName =
        locationsMap
        |> Map.tryFind configuredRegion
        |> Option.map(fun x -> x.Name |> shorten)
        |> Option.defaultValue (failwith "Missing or incorrect azure-native:location config value")

module Resource =
    open Pulumi.FSharp.Config
    
    let private woa =
        config.["workloadOrApplication"]
    
    let name resourceType (instanceNumber : int) =
        $"{resourceType}-{woa}-{Deployment.Instance.StackName}-{Region.shortName}-{instanceNumber:D3}"
        
    let nameOne resourceType =
        name resourceType 1