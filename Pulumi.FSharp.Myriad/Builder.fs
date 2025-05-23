module AstBuilder

open AstOperations
open AstInstance
open AstHelpers
open AstMember
open AstYield
open AstRun
open Core

open System.Text.RegularExpressions
open FSharp.Text.RegexProvider

// "azure:compute/virtualMachine:VirtualMachine"
// CloudProvider - Always the same for each schema (azure here)
type InfoProvider =
    Regex<"(?<CloudProvider>[a-z0-9-]+):(?<ResourceProviderNamespace>[A-Za-z0-9.]+)(/(?<SubNamespace>\w+))?:(?<ResourceType>\w+)">

let typeInfoProvider =
    InfoProvider(RegexOptions.Compiled)

type BuilderType =
    | Type of InfoProvider.MatchType
    | Resource of InfoProvider.MatchType

let private argIdent =
    Pat.ident("arg")
    
let private argToInput =
    Expr.app("input", "arg")
    
let private args =
    Expr.ident("args")
    
let private funcIdent =
    Expr.ident("func")
    
let private yieldReturnExpr =
    Expr.list([ Expr.ident("id") ])

let private combineExpr =
    Expr.app("_combine", "args")

let private combineCrosExpr =
    Expr.app("_combineCros", "args")

let private combineArgs =
    Pat.ident("args")
    
let private combineMember =
    createMember' None "this" "Combine" combineArgs [] combineExpr
    
let private combineCrosMember =
    createMember' None "this" "Combine" combineArgs [] combineCrosExpr
    
let private forArgs =
    Pat.paren (Pat.tuple ("args", "delayedArgs"))

let private forExpr =
    Expr.methodCall("this.Combine",
                    [ Expr.ident("args")
                      Expr.app("delayedArgs", Expr.unit) ])

let private forMember =
    createMember' None "this" "For" forArgs [] forExpr
 
let private delayMember =
    createMember "Delay" (Pat.ident("f")) [] (Expr.app("f", []))

let private zeroMember =
    createMember "Zero" Pat.wild [] Expr.unit
    
let private yieldMember isType =
    createYield isType yieldReturnExpr yieldReturnExpr Expr.null'
    
let private newNameExpr =
    Expr.tuple(Expr.ident("newName"),
               Expr.ident("args"),
               Expr.ident("cros"),
               Expr.ident("croI"))

let private nameMember =
    createNameOperation newNameExpr
    
let createYieldFor argsType propType =
    let setExpr =
        Expr.sequential([
            Expr.set("args." + propType.Name, argToInput)
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
    
    let cros =
        Expr.list([Expr.ident("id")])
    let croI = Expr.null'
        
    [ createYield' (not propType.IsResource) argIdent expr cros croI ]

let mapOperationType yieldSelector opsSelector =
    function
    | { Type = PRef _; CanGenerateYield = true } & pt -> yieldSelector pt
    | { Type = PRef _ }                          & pt
    | { Type = PString }                         & pt
    | { Type = PInteger }                        & pt
    | { Type = PFloat }                          & pt
    | { Type = PBoolean }                        & pt
    | { Type = PArray _ }                        & pt
    | { Type = PUnion _ }                        & pt
    | { Type = PJson _ }                         & pt
    | { Type = PMap _ }                          & pt
    | { Type = PAssetOrArchive _ }               & pt
    | { Type = PAny _ }                          & pt
    | { Type = PArchive _ }                      & pt -> opsSelector pt

let createBuilderClass isType name pTypes =
    let argsType =
        name + "Args"

    let croICheckNull () =
        Expr.match' (Expr.ident "croI",
            [Match.clause(Pat.null', createInstance "CustomResourceOptions" Expr.unit)
             Match.clause(Pat.ident "croI", Expr.ident "croI")])
        
    let apply varname =
        Expr.app("List.fold", [
            Expr.paren(Expr.lambda([ varname; "f" ], Expr.app("f", varname)))
            Expr.paren(match varname with | "args" -> createInstance argsType Expr.unit | _ -> croICheckNull())
            Expr.ident(varname)
        ])
       
    let resourceRunExp () =
        Expr.paren(
            Expr.tuple(
                Expr.ident("name"),
                (apply "args"),
                (apply "cros")
            )) |>
        createInstance name
        
    let createOperations =
        mapOperationType (createYieldFor argsType)
                         (createOperationsFor' argsType)
        
    let operations =
        pTypes |>
        Seq.collect createOperations
        
    let inputListOfInput argName =
        Expr.app(Expr.ident("inputList"),
                 Expr.list([Expr.app(Expr.ident("input"),
                                     Expr.ident(argName))]))
        
    let inputListOfResources argName =
        Expr.app(Expr.ident("inputList"),
                 Expr.paren(Expr.app(Expr.longIdent("Seq.map"),
                                     Expr.app(Expr.ident("input"),
                                              Expr.ident(argName)))))
        
    Module.type'(name + "Builder", [
        //Type.ctor()
        
        yieldMember isType
        
        if isType then
            apply "args"     |> createRunType
        else
            resourceRunExp() |> createRunResource
            
        if isType then combineMember else combineCrosMember
        forMember
        delayMember
        zeroMember
        
        yield! if isType then [] else [ nameMember ]
        
        yield! operations
        
        if not isType then
            croOperation "DependsOn"
                         "Obsolete. Use customResourceOptions nested CE. Ensure this resource gets created after its dependency"
                         "dependency"
                         (inputListOfInput "dependency")
                         true
                         
        if not isType then
            croOperation "DependsOn"
                         "Obsolete. Use customResourceOptions nested CE. Ensure this resource gets created after its dependency"
                         "dependency"
                         (inputListOfResources "dependency")
                         false
                         
        if not isType then
            createYield' false
                         (Pat.typed("croI", "CustomResourceOptions"))
                         (Expr.list([Expr.ident("id")]))
                         (Expr.list([Expr.ident("id")]))
                         (Expr.ident("croI"))
    ])