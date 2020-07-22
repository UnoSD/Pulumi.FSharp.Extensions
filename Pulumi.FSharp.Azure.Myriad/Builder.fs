module AstBuilder

open AstOperations
open FSharp.Data
open AstInstance
open AstHelpers
open AstMember
open AstYield
open AstRun
open FsAst
open Core

open System.Text.RegularExpressions
open FSharp.Text.RegexProvider

// "azure:compute/virtualMachine:VirtualMachine"
// CloudProvider - Always the same for each schema (azure here)
type ResourceInfoProvider =
    Regex<"(?<CloudProvider>\w+):(?<ResourceProviderNamespace>\w+)/(?<ResourceTypeCamelCase>\w+):(?<ResourceTypePascalCase>\w+)">

type TypeInfoProvider =
    Regex<"(?<CloudProvider>\w+):(?<ResourceProviderNamespace>\w+)/(?<ResourceType>\w+):(?<ResourceType2>\w+)">

let resourceInfo =
    ResourceInfoProvider(RegexOptions.Compiled)

let typeInfo =
    TypeInfoProvider(RegexOptions.Compiled)
    
type BuilderType =
    | Type of TypeInfoProvider.MatchType
    | Resource of ResourceInfoProvider.MatchType

let private argIdent =
    Pat.ident("arg")
    
let private argToInput =
    Expr.func("input", "arg")
    
let private args =
    Expr.ident("args")
    
let private funcIdent =
    Expr.ident("func")
    
let createAzureBuilderClass isType name properties (types : (TypeInfoProvider.MatchType * JsonValue) []) =
    let argsType =
        name + "Args"
       
    //let lambdaArg =
    //    SynSimplePats.SimplePats([ SynSimplePat.CreateTyped(Ident.Create("args"), SynType.Bool())
    //                               SynSimplePat.CreateTyped(Ident.Create("f"), SynType.Bool()) ], range.Zero)
        
    //let lambdaArg =
    //    SynSimplePats.SimplePats([], range.Zero)
    
    let apply =
        Expr.func("List.fold", [
            Expr.ident("func")
            Expr.paren(createInstance argsType Expr.unit)
            Expr.ident("args")
        ])
       
    let runArgs =
        if isType then
            apply
        else
            Expr.paren(
                Expr.tuple(
                    Expr.ident("name"),
                    Expr.paren(apply)
                ))
        
    // Infix: apply :: args, but produces: apply ``::`` args
    //let listCons =
    //    SynExpr.CreateApp(SynExpr.CreateAppInfix(SynExpr.CreateLongIdent(LongIdentWithDots.CreateString("::")),
    //                                             SynExpr.CreateIdentString("apply")),
    //                      SynExpr.CreateIdentString("args"))
        
    let createOperations propName (propType : string) =
        match propType with
        | "string"
        | "integer"
        | "number"
        | "boolean"
        | "array"
        | "object" ->
            createOperationsFor' isType propName propType argsType
        | _ -> // Complex
            let setExpr =
                Expr.sequential([
                    Expr.set("args." + propName, argToInput)
                    args
                ])
            
            let expr =
                Expr.list([
                    Expr.paren(
                        Expr.sequential([
                            Expr.let'("func", [Pat.typed("args", argsType)], setExpr)
                            funcIdent
                        ])
                    )
                ])
            
            [| createYield' argIdent expr |]
        
    let ctypes =
        types |>
        // Remove the inexisting type match
        Array.map fst
    
    let getComplexType typeFullPath =
        ctypes |>
        Array.tryFind (fun t -> ("#/types/" + t.ResourceType.Value) = typeFullPath) |>
        Option.map (fun x -> "complex:" + x.ResourceType.Value) |>
        Option.defaultValue "complex" // Should be only "pulumi.json#/" type
    
    let nameAndType name (properties : (string * JsonValue) []) =
        let tName =
            match properties |>
                  Array.tryFind (fun (p, _) -> p = "language") |>
                  Option.bind (fun (_, v) -> v.Properties() |>
                                             Array.tryFind (fun (p, _) -> p = "csharp") |>
                                             Option.map snd) |>
                  Option.map (fun v -> v.GetProperty("name").AsString()) with
            | Some name -> name
            | None      -> name
        
        let pType =
            properties |>
            Array.choose (fun (p, v) -> match p with
                                        | "type" -> v.AsString() |> Some // Array type has also "items"
                                        | "$ref" -> v.AsString() |> getComplexType |> Some
                                        (*| "description"*)
                                        | _ -> None) |>
            Array.head
        
        (tName |> toPascalCase, pType)
    
    let props =
        properties |>
        Array.map (fun (x, y : JsonValue) -> nameAndType x (y.Properties()))
        
    let operations =
        props |>
        Array.collect (fun (propName, propType) -> createOperations propName propType) |>
        List.ofArray
    
    let newNameExpr =
        Expr.tuple(Expr.ident("newName"),
                   Expr.ident("args"))
    
    let runReturnExpr =
        Expr.sequential([
            Expr.let'("func", [ "args"; "f" ], Expr.func("f", "args"))
            if isType then runArgs else createInstance name runArgs
        ])
    
    let yieldReturnExpr =
        Expr.list([ Expr.ident("id") ])
    
    let combineArgs =
        Pat.paren (Pat.tuple ((Pat.paren (Pat.tuple ("lName", "lArgs"))),
                              (Pat.paren (Pat.tuple ("rName", "rArgs")))))
    
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
                   Expr.paren(Expr.func("List.concat", (Expr.list [ "lArgs"; "rArgs" ]))))
    
    let createCombine() =
        createMember' "this" "Combine" [combineArgs.ToRcd] [] combineExpr
    
    let forArgs =
        Pat.paren (Pat.tuple ("args", "delayedArgs"))
    
    let forExpr =
        Expr.methodCall("this.Combine",
                        [ Expr.ident("args")
                          Expr.func("delayedArgs", Expr.unit) ])
    
    let createFor() =
        createMember' "this" "For" [forArgs.ToRcd] [] forExpr
     
    let createDelay() =
        createMember "Delay" [Pat.ident("f").ToRcd] [] (Expr.func("f"))
    
    let createZero() =
        createMember "Zero" [Pat.wild.ToRcd] [] (Expr.unit)
    
    Module.type'(name + "Builder", [
        Type.ctor()
        
        createYield yieldReturnExpr
        createRun (if isType then null else "name") runReturnExpr
        createCombine()
        createFor()
        createDelay()
        createZero()
        
        yield! if isType then [] else [ createNameOperation newNameExpr ]
        
        yield! operations
    ])