module AstOperations

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open AstAttribute
open AstHelpers
open AstMember
open FsAst
open Core
    
let private createPatternTyped name args (typeName : string) =
    SynPatRcd.CreateTyped(SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(name), args),
                          SynType.CreateLongIdent(typeName))

let private createTuple items withParen =
    if withParen then
        SynPatRcd.CreateParen(SynPatRcd.CreateTuple(items))
    else
        SynPatRcd.CreateTuple(items)

let private argsPattern =
    createPattern "args" []

let private nPattern =
    createPattern "n" []

let private namePattern =
    createPattern "name" []

let createPatternFromCache nameVarName =
    match nameVarName with
    | "n"    -> nPattern
    | "name" -> namePattern
    | _      -> createPattern nameVarName []

let argsTuple' nameVarName withParen =
    let nvn =
        match nameVarName with
        | null -> SynPatRcd.CreateWild
        | _    -> createPatternFromCache nameVarName
    
    createTuple [ nvn
                  argsPattern ] withParen

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

let private createOperation' isType name typeName hasAttribute =
    let nameArgName =
        if isType then
            "n"
        else
            "name"
    
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

let private listCons =
    Expr.funcTuple("List.Cons", [ "apply"; "args" ])

let private nReturnTuple =
    Expr.tuple(Expr.ident("n"), listCons)

let private nameReturnTuple =
    Expr.tuple(Expr.ident("name"), listCons)
    
// This should be the same as the member arg (currently "n")
let private returnTupleCache isType =
    match isType with
    | true  -> nReturnTuple
    | false -> nameReturnTuple

let private argsIdent =
    Expr.ident("args")

let private inputIdent =
    Expr.ident("input")

let private ioIdent =
    Expr.ident("io")

let private inputListIdent =
    Expr.ident("inputList")

let private inputMapIdent =
    Expr.ident("inputMap")

let private resourceNameIdent =
    Expr.ident("resourceName")

let createOperationsFor' isType name pType (argsType : string) =
    let setRights =
        match pType with
        | "string"
        | "integer"
        | "number"
        | "boolean" -> [ inputIdent; ioIdent ]
        | "array"   -> [ inputListIdent ]
        | "object"  -> [ inputMapIdent ]
        // What to do here? // I don't think complex exists at all... check and delete
        | "complex" -> [ inputIdent ]
        | x -> (name, x) ||> sprintf "Missing match case: %s, %s" |> failwith
    
    let letExpr setRight =
        Expr.let'("apply", [ Pat.typed("args", argsType) ],
                  Expr.sequential([
                      Expr.set("args." + name, SynExpr.CreateApp(setRight))
                      argsIdent
                  ]))
    
    let expr setExpr =
        Expr.sequential([
            setExpr
            returnTupleCache isType
        ])
    
    let operationName =
        match name with
        | "Name" when not isType -> resourceNameIdent
        | _                      -> Expr.ident(name |> toCamelCase)

    setRights |>
    List.map ((fun sr -> sr, operationName) >> letExpr >> expr) |>
    List.mapi (fun i e -> createOperation' isType name (i = 0) e)