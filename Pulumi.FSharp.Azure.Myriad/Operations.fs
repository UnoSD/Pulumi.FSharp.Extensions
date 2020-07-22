module AstOperations

open FSharp.Compiler.Range
open FSharp.Compiler.SyntaxTree
open Core
open AstMember
open AstAttribute
open FsAst
open AstYield
open AstHelpers
    
let private createPatternTyped name args (typeName : string) =
    SynPatRcd.CreateTyped(SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(name), args),
                          SynType.CreateLongIdent(typeName))

let private createTuple items withParen =
    if withParen then
        SynPatRcd.CreateParen(SynPatRcd.CreateTuple(items))
    else
        SynPatRcd.CreateTuple(items)

let argsTuple' nameVarName withParen =
    let nvn =
        match nameVarName with
        | null -> SynPatRcd.CreateWild
        | _    -> createPattern nameVarName []
    
    createTuple [ nvn
                  createPattern "args" [] ] withParen

let private createOperation'' nameVarName name coName argName hasAttribute =
    let attributes =
        if hasAttribute then
            [ createAttributeWithArg "CustomOperation" coName ]
        else
            []
    
    let patterns = [
        createTuple [
            argsTuple' nameVarName true
            createPattern argName []
        ] true
    ]
    
    createMember name patterns attributes

let private createOperation' nameArgName isType name typeName hasAttribute =
    let snakeCaseName =
        if name = "Name" && (not isType) then
            "resourceName"
        else 
            toCamelCase name
    
    let coName =
        match snakeCaseName with
        | "resourceGroupName" -> "resourceGroup"
        | "name" when not isType -> "resourceName"
        | x -> x
    
    createOperation'' nameArgName (coName |> toPascalCase) coName snakeCaseName typeName hasAttribute

let createNameOperation newNameExpr =
    createOperation'' null "Name" "name" "newName" true newNameExpr    

let createOperationsFor' isType name pType (argsType : string) tupleArgs =
    let setRights =
        match pType with
        | "string"
        | "integer"
        | "number"
        | "boolean" -> [ SynExpr.CreateIdentString("input"); SynExpr.CreateIdentString("io") ]
        | "array" -> [ SynExpr.CreateIdentString("inputList") ]
        | "object" -> [ SynExpr.CreateIdentString("inputMap") ]
        // What to do here? // I don't think complex exists at all... check and delete
        | "complex" -> [ SynExpr.CreateIdentString("input") ]
        | x -> (name, x) ||> sprintf "Missing match case: %s, %s" |> failwith
    
    let setExpr setRight =
        SynExpr.CreateSequential([
            SynExpr.Set (SynExpr.CreateLongIdent(LongIdentWithDots.CreateString("args." + name)),
                         SynExpr.CreateApp(setRight),
                         range.Zero)
            SynExpr.CreateIdentString("args")])
    
    let letExpr setRight =
        Expr.let'("apply", [ (Pat.typed("args", argsType)) ], setExpr setRight)
    
    // This should be the same as the member arg (currently "n")
    let nameArgName =
        if isType then
            "n"
        else
            "name"
    
    let expr setExpr =
        SynExpr.CreateSequential([
            setExpr
            SynExpr.CreateTuple(tupleArgs (SynExpr.CreateIdentString(nameArgName)))
        ])
        
    setRights |>
    List.map (fun sr -> sr, SynExpr.CreateIdentString(match name with | "Name" when not isType -> "resourceName" | _ -> name |> toCamelCase)) |>
    List.map (letExpr >> expr) |>
    List.mapi (fun i e -> createOperation' nameArgName isType name (i = 0) e) |>
    Array.ofList
    
let private argIdent =
    Pat.ident("arg")
    
let private argToInput =
    Expr.func("input", "arg")
    
let private args =
    Expr.ident("args")
    
let private funcIdent =
    Expr.ident("func")
    
let createOperationsFor isType name (pType : string) argsType tupleArgs =
    let setExpr =
        Expr.sequential([
            Expr.set("args." + name, argToInput)
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
    
    if pType.StartsWith("complex:") then
        [| createYield' argIdent expr |]
    else 
        createOperationsFor' isType name pType argsType tupleArgs