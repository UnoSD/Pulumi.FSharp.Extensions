module private Kubernetes

let private config = {|
    Version = "2.4.0" // Version needs to match NuGet package Pulumi.Kubernetes
    ChangeValueToForceReGeneration = false
|}