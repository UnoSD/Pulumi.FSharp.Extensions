namespace Pulumi.FSharp.Azure

module Regions =    
    // Get list from Azure REST API (find anonymous endpoint)
    // Create type provider to convert to discriminated union the response of:
    // https://management.azure.com/subscriptions/{subscriptionId}/locations?api-version=2020-01-01
    type Region =
        | EastUS
        | EastUS2
        | SouthCentralUS
        | WestUS2
        | AustraliaEast
        | SoutheastAsia
        | NorthEurope
        | UKSouth
        | WestEurope
        | CentralUS
        | NorthCentralUS
        | WestUS
        | SouthAfricaNorth
        | CentralIndia
        | EastAsia
        | JapanEast
        | KoreaCentral
        | CanadaCentral
        | FranceCentral
        | GermanyWestCentral
        | NorwayEast
        | SwitzerlandNorth
        | UAENorth
        | BrazilSouth
        | CentralUSStage
        | EastUS2Stage
        | NorthCentralUSStage
        | SouthCentralUSStage
        | WestUSStage
        | WestUS2Stage
        | AsiaPacific
        | Australia
        | Europe
        | Global
        | Japan
        | UK
        | UnitedStates
        | EastAsiaStage
        | CentralUSEUAP
        | EastUS2EUAP
        | WestCentralUS
        | SouthAfricaWest
        | AustraliaCentral
        | AustraliaCentral2
        | AustraliaSoutheast
        | JapanWest
        | KoreaSouth
        | SouthIndia
        | WestIndia
        | CanadaEast
        | FranceSouth
        | GermanyNorth
        | NorwayWest
        | SwitzerlandWest
        | UKWest
        | UAECentral
    
    // Get names from union type cases formatted instead (or provided by the REST API above)
    let regionName =
        function
        | EastUS -> "eastus"
        | EastUS2 -> "eastus2"
        | SouthCentralUS -> "southcentralus"
        | WestUS2 -> "westus2"
        | AustraliaEast -> "australiaeast"
        | SoutheastAsia -> "southeastasia"
        | NorthEurope -> "northeurope"
        | UKSouth -> "uksouth"
        | WestEurope -> "westeurope"
        | CentralUS -> "centralus"
        | NorthCentralUS -> "northcentralus"
        | WestUS -> "westus"
        | SouthAfricaNorth -> "southafricanorth"
        | CentralIndia -> "centralindia"
        | EastAsia -> "eastasia"
        | JapanEast -> "japaneast"
        | KoreaCentral -> "koreacentral"
        | CanadaCentral -> "canadacentral"
        | FranceCentral -> "francecentral"
        | GermanyWestCentral -> "germanywestcentral"
        | NorwayEast -> "norwayeast"
        | SwitzerlandNorth -> "switzerlandnorth"
        | UAENorth -> "uaenorth"
        | BrazilSouth -> "brazilsouth"
        | CentralUSStage -> "centralusstage"
        | EastUS2Stage -> "eastus2stage"
        | NorthCentralUSStage -> "northcentralusstage"
        | SouthCentralUSStage -> "southcentralusstage"
        | WestUSStage -> "westusstage"
        | WestUS2Stage -> "westus2stage"
        | AsiaPacific -> "asiapacific"
        | Australia -> "australia"
        | Europe -> "europe"
        | Global -> "global"
        | Japan -> "japan"
        | UK -> "uk"
        | UnitedStates -> "unitedstates"
        | EastAsiaStage -> "eastasiastage"
        | CentralUSEUAP -> "centraluseuap"
        | EastUS2EUAP -> "eastus2euap"
        | WestCentralUS -> "westcentralus"
        | SouthAfricaWest -> "southafricawest"
        | AustraliaCentral -> "australiacentral"
        | AustraliaCentral2 -> "australiacentral2"
        | AustraliaSoutheast -> "australiasoutheast"
        | JapanWest -> "japanwest"
        | KoreaSouth -> "koreasouth"
        | SouthIndia -> "southindia"
        | WestIndia -> "westindia"
        | CanadaEast -> "canadaeast"
        | FranceSouth -> "francesouth"
        | GermanyNorth -> "germanynorth"
        | NorwayWest -> "norwaywest"
        | SwitzerlandWest -> "switzerlandwest"
        | UKWest -> "ukwest"
        | UAECentral -> "uaecentral"