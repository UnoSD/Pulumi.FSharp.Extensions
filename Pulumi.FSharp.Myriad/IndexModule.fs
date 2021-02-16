module IndexModule

open AstHelpers
open AstModules

let createIndexModule name (modules : PulumiModule list) =
    let (indexTypes, qualifiedTypes) =
        List.partition (function
                        | { ResourceProviderNamespace = None } -> true
                        | _                                    -> false) modules

    Module.module'(name, [
        Module.open'($"Pulumi.{name}")
        
        yield! indexTypes |> List.map (fun x -> x.Content) |> Array.concat
        
        yield! qualifiedTypes |> List.map (fun x -> createModule x.ResourceProviderNamespace x.CloudProviderNamespace x.Content)
    ])