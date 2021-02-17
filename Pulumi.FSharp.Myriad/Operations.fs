module AstOperations

open FSharp.Compiler.SyntaxTree
open AstAttribute
open AstHelpers
open AstMember
open FsAst
open Core
    
type PropertyType =
    | Json
    | Array
    | ArrayItems of PropertyType[]
    | Union of PropertyType[] * PropertyType[]
    | String
    | Integer
    | Float
    | Boolean
    | Map
    | MapType of PropertyType
    | Description of string
    | DeprecationMessage of string
    | Complex of string
    | NameOverride of string
    | Asset
    | Ignore
    
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

let private createOperation'' nameVarName name coName argName hasAttribute typ =
    let attributes =
        if hasAttribute then
            [ createAttributeWithArg "CustomOperation" coName ]
        else
            []
    
    let patterns = [
        createTuple [
            argsTuple' nameVarName true
            match typ with | None -> createPattern argName [] | Some typ -> createPatternTyped argName [] typ
        ] true
    ]
    
    createMember name patterns attributes

let createNameOperation newNameExpr =
    createOperation'' null "Name" "name" "newName" true None newNameExpr

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

let private inputJson =
    Expr.ident("Pulumi.InputJson.op_Implicit")

let private resourceNameIdent =
    Expr.ident("resourceName")

let private compose =
    Expr.paren(Expr.ident("op_ComposeRight"))

let private inputListFromSeqOf (expr : SynExpr) =
    Expr.paren(
        Expr.func(compose, [
            Expr.paren(Expr.func(Expr.longIdent("Seq.map"), expr))
            inputListIdent
        ])
    )

let private inputListFromSeq =
    inputListFromSeqOf inputIdent

let private inputListFromOutputSeq =
    inputListFromSeqOf ioIdent

let private inputListFromItemOf (expr : SynExpr) =
    Expr.paren(Expr.func(compose, (Expr.func(Expr.paren(Expr.func(compose, [ expr; Expr.longIdent("Seq.singleton") ])), inputListIdent))))

let private inputListFromItem =
    inputListFromItemOf inputIdent
    
let private inputListFromOutput =
    inputListFromItemOf ioIdent

let private inputUnion1Of2 =
    Expr.ident("inputUnion1Of2")

let private inputUnion2Of2 =
    Expr.ident("inputUnion2Of2")

let private idIdent =
    Expr.ident("id")

let createOperationsFor' isType name pType (argsType : string) =
    let (setRights, argType) =
        match pType with
        | [| String |]
        //| "integer"
        //| "number"
        //| "boolean" -> [ inputIdent; ioIdent ], None
        | [| Array _ |] -> [ inputListIdent; inputListFromSeq; inputListFromOutputSeq; inputListFromItem; inputListFromOutput ], None
        | [| Union _ |] -> [ idIdent; inputUnion1Of2; inputUnion2Of2 ], None
        | [| Json |] -> [ inputJson ], Some "string"
        //| "object"  -> [ inputMapIdent ], None
        // What to do here? // I don't think complex exists at all... check and delete
        //| "complexD"
        //| "complex" -> [ inputIdent ], None
        | x -> (name, x) ||> sprintf "Missing match case: %s, %A" |> failwith
    
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

    let snakeCaseName =
        if name = "Name" && (not isType) then
            "resourceName"
        else 
            toCamelCase name
    
    let argName =
        match snakeCaseName with
        | "input"
        | "args" -> snakeCaseName + "\'"
        | _      -> snakeCaseName
    
    let operationName =
        match name with
        | "Name" when not isType -> resourceNameIdent
        | _                      -> Expr.ident(argName)
    
    let nameArgName =
        if isType then
            "n"
        else
            "name"
    
    let coName =
        match snakeCaseName with
        | "resourceGroupName" -> "resourceGroup"
        | "name" when not isType -> "resourceName"
        | x -> x
    
    let memberName =
        coName |> toPascalCase
    
    setRights |>
    List.map ((fun sr -> sr, operationName) >> letExpr >> expr) |>
    List.mapi (fun i e -> createOperation'' nameArgName memberName coName argName (i = 0) argType e)