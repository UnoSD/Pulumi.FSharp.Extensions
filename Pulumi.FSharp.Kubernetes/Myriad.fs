module private Kubernetes

let private config = {|
    Version = "2.4.1" // Version needs to match NuGet package Pulumi.Kubernetes
|}

module Force = let private nonce = 30171