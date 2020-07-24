module private Azure

let private config = {|
    Version = "3.11.0" // Version needs to match NuGet package Pulumi.Azure
    ChangeValueToForceReGeneration = false
|}