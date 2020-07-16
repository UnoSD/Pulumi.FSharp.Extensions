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

let private argsTuple' nameVarName withParen =
    let nvn =
        match nameVarName with
        | null -> SynPatRcd.CreateWild
        | _    -> createPattern nameVarName []
    
    createTuple [ nvn
                  createPattern "args" [] ] withParen

let argsTuple =
    argsTuple' "name"

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

let createOperation =
    createOperation'' "name"

let private createOperation' name typeName hasAttribute =
    let snakeCaseName =
        if name = "Name" then
            "resourceName"
        else 
            toSnakeCase name
    
    let coName =
        match snakeCaseName with
        | "resourceGroupName" -> "resourceGroup"
        | "name" -> "resourceName"
        | x -> x
    
    createOperation (coName |> toPascalCase) coName snakeCaseName typeName hasAttribute

let createNameOperation newNameExpr =
    createOperation'' null "Name" "name" "newName" true newNameExpr    

let createOperationsFor' name pType (argsType : string) tupleArgs =
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
    
    let expr setExpr =
        SynExpr.CreateSequential([
            setExpr
            SynExpr.CreateTuple(tupleArgs (SynExpr.CreateIdentString("name")))
        ])
        
    setRights |>
    List.map (fun sr -> sr, SynExpr.CreateIdentString(match name with | "Name" -> "resourceName" | _ -> name |> toSnakeCase)) |>
    List.map (letExpr >> expr) |>
    List.mapi (fun i e -> createOperation' name (i = 0) e) |>
    Array.ofList
    
let createOperationsFor name (pType : string) argsType tupleArgs =
    let setExpr =
        SynExpr.CreateSequential([
            SynExpr.Set (SynExpr.CreateLongIdent(LongIdentWithDots.CreateString("args." + name)),
                         SynExpr.CreateApp(Expr.ident("input"), Expr.ident("arg")),
                         range.Zero)
            SynExpr.CreateIdentString("args")])
    
    let expr typeName =
        Expr.paren(
            Expr.sequential([
                Expr.let'("func", [Pat.typed("args", argsType)], setExpr)
                Expr.ident("func")
            ])      
        )
    
    if pType.StartsWith("complex:") then
        [| createYield' (Pat.ident("arg")) (Expr.list([ expr (pType.Substring(8)) ])) |]
    else 
        createOperationsFor' name pType argsType tupleArgs