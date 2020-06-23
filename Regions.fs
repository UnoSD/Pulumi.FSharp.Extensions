namespace Pulumi.FSharp.Azure

module Core =    
    // Get list from Azure REST API
    type Region =
        | WestEurope
    
    // Get names from union type cases formatted instead
    let regionName =
        function
        | WestEurope -> "West Europe"