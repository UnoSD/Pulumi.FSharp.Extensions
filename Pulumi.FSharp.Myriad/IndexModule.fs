module IndexModule

open FSharp.Compiler.SyntaxTree
open AstHelpers
open AstModules
open Core

type private Namespace =
    {
        Name: string
        SubNamespaceName: string option
        Content: seq<SynModuleDecl>
    }

type private SubNamespace =
    {
        Name: string option
        Content: seq<SynModuleDecl>
    }

let createModules provider ((indexTypes, qualifiedTypes) : PulumiModule list * PulumiModule list) =
    let groupSub contentList =
        contentList |>
        List.groupBy (fun ns -> ns.SubNamespaceName) |>
        List.map (fun (subName, content) -> { Name = subName; Content = content |> Seq.collect (fun c -> c.Content) })

    let createSubmodule subName rootNamespace content =
        createModule' subName [$"{provider}.{rootNamespace}.{subName}"
                               $"{provider}.Types.Inputs.{rootNamespace}.{subName}"] content
    
    let createSubmodules rootNamespace =
        Seq.collect (function
                     | { Content = content
                         SubNamespace.Name = None } -> content
                     | { Content = content
                         Name = Some subName }      -> seq { createSubmodule subName rootNamespace content })
    
    let qualifiedTypesModules =
        qualifiedTypes |>
        List.map ((fun qualifiedTypeModule -> (String.split '.' qualifiedTypeModule.ResourceProviderNamespace.Value,
                                               qualifiedTypeModule.Content)) >>
                  (function
                   | ([|rootNamespace|], content) -> { Name = rootNamespace
                                                       SubNamespaceName = None
                                                       Content = content }
                   | ([|rootNamespace; subNamespace|], content) -> { Name = rootNamespace
                                                                     SubNamespaceName = Some subNamespace
                                                                     Content = content }
                   | _ -> failwith "Too many nested namespaces")) |>
        List.groupBy (fun rootNamespace -> rootNamespace.Name) |>
        List.map (fun (rootNamespaceName, content) -> groupSub content |>
                                                      createSubmodules rootNamespaceName |>
                                                      createModule' rootNamespaceName [$"{provider}.{rootNamespaceName}"])
    
    let indexTypesAsts =
        indexTypes |> Seq.collect (fun x -> x.Content)
    
    Module.module'(provider, [
        Module.open'($"Pulumi.{provider}")
        
        yield! indexTypesAsts
        
        yield! qualifiedTypesModules
    ])