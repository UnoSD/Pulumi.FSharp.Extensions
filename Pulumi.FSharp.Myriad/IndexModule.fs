module IndexModule

open FSharp.Compiler.Syntax
open AstHelpers
open AstModules
open FSharp.Compiler.Text
open Myriad.Core.Ast
open Myriad.Core.AstExtensions
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
                   | [|rootNamespace|], content -> { Name = rootNamespace
                                                     SubNamespaceName = None
                                                     Content = content }
                   | [|rootNamespace; subNamespace|], content -> { Name = rootNamespace
                                                                   SubNamespaceName = Some subNamespace
                                                                   Content = content }
                   | _ -> failwith "Too many nested namespaces")) |>
        List.groupBy (fun rootNamespace -> rootNamespace.Name) |>
        List.map (fun (rootNamespaceName, content) -> groupSub content |>
                                                      createSubmodules rootNamespaceName |>
                                                      createModule' rootNamespaceName [$"{provider}.{rootNamespaceName}"])
    
    let indexTypesAsts =
        indexTypes |> Seq.collect (fun x -> x.Content)
    
    let letCombineImplementation = 
        let fromRcd =
            SynPat.CreateLongIdent(LongIdentWithDots.CreateString("_combine"),[
                Pat.paren(Pat.tuple(Pat.paren(Pat.tuple("rName", "rArgs")),
                                    Pat.paren(Pat.tuple("lName", "lArgs"))))
            ]) |> ignore // Replace below with this
            
            SynPat.CreateLongIdent(LongIdentWithDots.CreateString("_combine"), [
                SynPat.CreateParen(SynPat.Tuple(false, [
                    SynPat.CreateParen(SynPat.Tuple(false, [ SynPat.CreateLongIdent(LongIdentWithDots.CreateString("rName"), [])
                                                             SynPat.CreateLongIdent(LongIdentWithDots.CreateString("rArgs"), []) ], range.Zero))
                    SynPat.CreateParen(SynPat.Tuple(false, [ SynPat.CreateLongIdent(LongIdentWithDots.CreateString("lName"), [])
                                                             SynPat.CreateLongIdent(LongIdentWithDots.CreateString("lArgs"), []) ], range.Zero))
                ], Range.Zero))
            ])

        let matchExpr =
            Expr.paren(
                Expr.match'(Expr.tuple(Expr.ident("lName"), Expr.ident("rName")), [
                    Match.clause(Pat.tuple(Pat.null', Pat.null'), Expr.null')
                    Match.clause(Pat.tuple(Pat.null', Pat.ident("name")), Expr.ident("name"))
                    Match.clause(Pat.tuple(Pat.ident("name"), Pat.null'), Expr.ident("name"))
                    Match.clause(Pat.wild, Expr.failwith("Duplicate name"))
                ]))

        let combineExpr =
            Expr.tuple(matchExpr,
                       Expr.paren(Expr.app("List.concat", (Expr.list [ "lArgs"; "rArgs" ]))))
            
        let expr =
            combineExpr

        SynModuleDecl.CreateLet([
            SynBinding.Let(SynAccess.Private, pattern = fromRcd, expr = expr)
        ])
    
    let letCombineCrosImplementation = 
        let fromRcd =
            SynPat.CreateLongIdent(LongIdentWithDots.CreateString("_combineCros"),[
                Pat.paren(Pat.tuple(Pat.paren(Pat.tuple("rName", "rArgs", "rCros")),
                                    Pat.paren(Pat.tuple("lName", "lArgs", "lCros"))))
            ])

        let matchExpr =
            Expr.paren(
                Expr.match'(Expr.tuple(Expr.ident("lName"), Expr.ident("rName")), [
                    Match.clause(Pat.tuple(Pat.null', Pat.null'), Expr.null')
                    Match.clause(Pat.tuple(Pat.null', Pat.ident("name")), Expr.ident("name"))
                    Match.clause(Pat.tuple(Pat.ident("name"), Pat.null'), Expr.ident("name"))
                    Match.clause(Pat.wild, Expr.failwith("Duplicate name"))
                ]))

        let combineExpr =
            Expr.tuple(matchExpr,
                       Expr.paren(Expr.app("List.concat", (Expr.list [ "lArgs"; "rArgs" ]))),
                       Expr.paren(Expr.app("List.concat", (Expr.list [ "lCros"; "rCros" ]))))
            
        let expr =
            combineExpr

        SynModuleDecl.CreateLet([
            SynBinding.Let(SynAccess.Private, pattern = fromRcd, expr = expr)
        ])
    
    Module.module'(provider, [
        Module.open'($"Pulumi.{provider}")
        
        letCombineImplementation
        
        letCombineCrosImplementation
        
        yield! indexTypesAsts
        
        yield! qualifiedTypesModules
    ])