module private Aws

let private config = {|
    Version = "3.28.1" // Version needs to match NuGet package Pulumi.Aws
|}

module Force = let private nonce = 30171