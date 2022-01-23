module AstBuilder

open AstOperations
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
type InfoProvider =
    Regex<"(?<CloudProvider>[a-z-]+):(?<ResourceProviderNamespace>[A-Za-z0-9.]+)(/(?<SubNamespace>\w+))?:(?<ResourceType>\w+)">

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
    createMember' None "this" "Combine" [combineArgs.ToRcd] [] combineExpr
    
let private combineCrosMember =
    createMember' None "this" "Combine" [combineArgs.ToRcd] [] combineCrosExpr
    
let private forArgs =
    Pat.paren (Pat.tuple ("args", "delayedArgs"))

let private forExpr =
    Expr.methodCall("this.Combine",
                    [ Expr.ident("args")
                      Expr.app("delayedArgs", Expr.unit) ])

let private forMember =
    createMember' None "this" "For" [forArgs.ToRcd] [] forExpr
 
let private delayMember =
    createMember "Delay" [Pat.ident("f").ToRcd] [] (Expr.app("f", []))

let private zeroMember =
    createMember "Zero" [Pat.wild.ToRcd] [] Expr.unit
    
let private yieldMember isType =
    createYield isType yieldReturnExpr yieldReturnExpr
    
let private newNameExpr =
    Expr.tuple(Expr.ident("newName"),
               Expr.ident("args"),
               Expr.ident("cros"))

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
    
    [ createYield' (not propType.IsResource) argIdent expr cros ]

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

    let apply varname =
        Expr.app("List.fold", [
            Expr.paren(Expr.lambda([ varname; "f" ], Expr.app("f", varname)))
            Expr.paren(createInstance (match varname with | "args" -> argsType | _ -> "CustomResourceOptions") Expr.unit)
            Expr.ident(varname)
        ])
       
    let resourceRunExp () =
        Expr.paren(
            Expr.tuple(
                Expr.ident("name"),
                Expr.paren(apply "args"),
                Expr.paren(apply "cros")
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
        
    Module.type'(name + "Builder", [
        Type.ctor()
        
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
                         "Ensure this resource gets created after its dependency"
                         "dependency"
                         (inputListOfInput "dependency")
    ])