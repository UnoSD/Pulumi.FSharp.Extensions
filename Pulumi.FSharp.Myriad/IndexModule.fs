module IndexModule

open AstHelpers
open AstModules
open Core

let createIndexModule name (modules : PulumiModule list) =
    let (indexTypes, qualifiedTypes) =
        List.partition (function
                        | { ResourceProviderNamespace = None } -> true
                        | _                                    -> false) modules

    let cloudProviderNamespace = modules |> List.map (fun x -> x.CloudProviderNamespace) |> List.distinct |> List.exactlyOne
    
    let groupSub (contentList : {| Content: FSharp.Compiler.SyntaxTree.SynModuleDecl []; Root: string; Sub: string option |} list) =
        contentList |>
        List.groupBy (fun x -> x.Sub) |>
        List.map (fun (sub, contentList) -> {| Name = sub; Content = contentList |> Seq.collect (fun c -> c.Content) |})
    
    let qualifiedTypesGrouped =
        qualifiedTypes |>
        List.map (fun qualifiedTypeModule -> (String.split '.' qualifiedTypeModule.ResourceProviderNamespace.Value, qualifiedTypeModule.Content)) |>
        List.map (function
                  | ([|rootNamespace|], content) -> {| Root = rootNamespace; Sub = None; Content = content |}
                  | ([|rootNamespace; subNamespace|], content) -> {| Root = rootNamespace; Sub = Some subNamespace; Content = content |}
                  | _ -> failwith "Too many nested namespaces") |>
        List.groupBy (fun r -> r.Root) |>
        List.map (fun (rootNamespace, contentList) -> {| Root = rootNamespace; Subs = groupSub contentList |})
    
    let createSubmodules rootNamespace (subs : {| Content: seq<FSharp.Compiler.SyntaxTree.SynModuleDecl>; Name: string option |} list) : seq<FSharp.Compiler.SyntaxTree.SynModuleDecl> =
        subs |> Seq.collect (fun sub -> match sub.Name with
                                        | Some subName -> seq { createModule' subName [$"{cloudProviderNamespace}.{rootNamespace}.{subName}"
                                                                                       $"{cloudProviderNamespace}.Types.Inputs.{rootNamespace}.{subName}"] sub.Content }
                                        | None         -> sub.Content)
    
    let qualifiedTypesModules =
        qualifiedTypesGrouped |>
        List.map (fun x -> createSubmodules x.Root x.Subs |> createModule' x.Root [$"{cloudProviderNamespace}.{x.Root}"])
    
    Module.module'(name, [
        Module.open'($"Pulumi.{name}")
        
        yield! indexTypes |> List.map (fun x -> x.Content) |> Array.concat
        
        yield! qualifiedTypesModules
    ])