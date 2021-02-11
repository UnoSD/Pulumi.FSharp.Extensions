module private Azure

let private config = {|
    Version = "3.46.0" // Version needs to match NuGet package Pulumi.Azure
|}

module Force = let private nonce = 30171