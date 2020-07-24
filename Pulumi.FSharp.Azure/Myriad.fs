module private Azure

let private config = {|
    Version = "3.12.1" // Version needs to match NuGet package Pulumi.Azure
    ChangeValueToForceReGeneration = false
|}