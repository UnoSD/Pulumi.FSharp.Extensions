module AstOperations

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.XmlDoc
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

let private crosPattern =
    createPattern "cros" []

let private nPattern =
    createPattern "n" []

let private namePattern =
    createPattern "name" []

let createPatternFromCache nameVarName =
    match nameVarName with
    | "n"    -> nPattern
    | "name" -> namePattern
    | _      -> createPattern nameVarName []

let argsTuple' isResource nameVarName withParen =
    let nvn =
        match nameVarName with
        | null -> SynPatRcd.CreateWild
        | _    -> createPatternFromCache nameVarName
    
    createTuple [ nvn
                  argsPattern
                  if isResource then crosPattern ] withParen

let argsTupleResource withParen =
    createTuple [ namePattern
                  argsPattern
                  crosPattern ] withParen

let argsTupleType withParen =
    createTuple [ SynPatRcd.CreateWild
                  argsPattern ] withParen

let private createOperation'' isResource (xmlDoc : string list) nameVarName name coName argName hasAttribute typ =
    let attributes =
        if hasAttribute then
            [ createAttributeWithArg "CustomOperation" coName ]
        else
            []
    
    let patterns = [
        createTuple [
            argsTuple' isResource nameVarName true
            match typ with | None -> createPattern argName [] | Some typ -> createPatternTyped argName [] typ
        ] true
    ]
    
    let doc =
        PreXmlDoc.Create(xmlDoc) |> Some
    
    createMember'' doc name patterns attributes

let createNameOperation newNameExpr =
    createOperation'' true ["Pulumi logical resource name"] null "Name" "name" "newName" true None newNameExpr

let private listCons =
    Expr.appTuple("List.Cons", [ "apply"; "args" ])

let private nReturnTuple =
    Expr.tuple(Expr.ident("n"), listCons)

let private nameReturnTuple =
    Expr.tuple(Expr.ident("name"), listCons)

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
    Expr.longIdent("Pulumi.InputJson.op_Implicit")

let private resourceNameIdent =
    Expr.ident("resourceName")

let private resourceTypeIdent =
    Expr.ident("resourceType")

let private compose =
    Expr.paren(Expr.ident("op_ComposeRight"))

let private inputListFromSeqOf (expr : SynExpr) =
    Expr.paren(
        Expr.app(compose, [
            Expr.paren(Expr.app(Expr.longIdent("Seq.map"), expr))
            inputListIdent
        ])
    )

let private inputMapFromMapOf (expr : SynExpr) =    
    let mapSelector =
        Expr.paren(
            Expr.lambda([ SimplePat.id("k")
                          SimplePat.id("v") ],
                        Expr.tuple(Expr.ident("k"),
                                   Expr.app(expr,
                                            Expr.ident("v")))))
    
    Expr.paren(
        Expr.app(compose, [
            Expr.paren(Expr.app(Expr.longIdent("Seq.map"), mapSelector))
            inputMapIdent
        ])
    )

let private inputMapFromMapOfInput =
    inputMapFromMapOf inputIdent

let private inputMapFromMapOfOutput =
    inputMapFromMapOf ioIdent

let private inputListFromSeq =
    inputListFromSeqOf inputIdent

let private inputListFromOutputSeq =
    inputListFromSeqOf ioIdent

let private inputListFromItemOf (expr : SynExpr) =
    Expr.paren(Expr.app(compose,
                        (Expr.app(Expr.paren(Expr.app(compose,
                                                      [ expr; Expr.longIdent("Seq.singleton") ])), inputListIdent))))

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

type PType =
    | PArray of PType
    | PUnion of PType * PType
    | PString 
    | PInteger
    | PFloat  
    | PBoolean
    | PMap of PType  
    | PJson   
    | PAssetOrArchive
    | PArchive
    | PAny
    | PRef of string
    
type Deprecation =
    | Current
    | Deprecated of string
    
type PTypeDefinition =
    {
        Name: string
        Type: PType
        Description: string
        Deprecation: Deprecation
        CanGenerateYield: bool
        IsResource: bool
        OperationName: string
    }

// This should be the same as the member arg (currently "n")
let private returnTupleCache argsType pType opName setRight =    
    let set =
        Expr.set($"args.{pType.Name}", setRight)
    
    let lambdaExpr =
        Expr.sequential([
            set
            Expr.ident("args")
        ])
    
    let lambda =
        Expr.lambda([
            SimplePat.typed("args", argsType)
        ], lambdaExpr)
    
    let consArg =
        Expr.paren(Expr.tuple(Expr.paren(lambda), Expr.ident("args")))
    
    let cons =
        Expr.app(Expr.longIdent("List.Cons"), consArg)
    
    match pType.IsResource with    
    | false -> Expr.tuple(Expr.ident("n"), cons)
    | true  -> Expr.tuple(Expr.ident("name"), cons, Expr.ident("cros"))

let createOperationsFor' argsType pType =
    let (setRights, argType) =
        match pType with
        | { PTypeDefinition.Type = PString }
        | { Type = PInteger }
        | { Type = PFloat }
        | { Type = PBoolean } -> [ inputIdent; ioIdent ], None
        | { Type = PArray _ } -> [ inputListIdent; inputListFromSeq; inputListFromOutputSeq; inputListFromItem; inputListFromOutput ], None
        | { Type = PUnion _ } -> [ idIdent; inputUnion1Of2; inputUnion2Of2 ], None
        | { Type = PJson }    -> [ inputJson ], Some "string"
        | { Type = PMap _ }   -> [ idIdent; inputMapIdent; inputMapFromMapOfInput; inputMapFromMapOfOutput ], None
        | { Type = PRef _ }
        | { Type = PArchive }
        | { Type = PAny }
        | { Type = PAssetOrArchive } -> [ inputIdent ], None
    
    let snakeCaseName =
        if pType.Name = "Name" && pType.IsResource then
            "resourceName"
        else 
            toCamelCase pType.Name
    
    let argName =
        match snakeCaseName with
        | "input"
        | "args" -> snakeCaseName + "\'"
        | "type" -> "resourceType"
        | _      -> snakeCaseName
    
    let operationName =
        match pType.Name with
        | "Name" when pType.IsResource -> resourceNameIdent
        | "Type" when pType.IsResource -> resourceTypeIdent
        | _                            -> Expr.ident(argName)
    
    let nameArgName =
        if pType.IsResource then
            "name"
        elif pType.OperationName = "n" then
            "nx"
        else
            "n"
    
    let memberName =
        pType.OperationName |> toPascalCase
    
    let doc =
        String.split '\n' pType.Description |> Array.filter (((=)"") >> not) |> List.ofArray
    
    let returnTupleCache' =
        returnTupleCache argsType pType operationName
    
    let argNameExpr =
        Expr.ident(argName)
    
    setRights |>
    List.map ((fun sr -> Expr.app(sr, argNameExpr)) >> returnTupleCache') |>
    List.mapi (fun i e -> createOperation'' pType.IsResource doc nameArgName memberName pType.OperationName argName (i = 0) argType e)
    
    
let croOperation operationName description argumentName (setAssignmentExpression : SynExpr) =
    let attributes =
        [ createAttributeWithArg "CustomOperation" (operationName |> toCamelCase) ]
    
    let patterns = [
        createTuple [
            argsTupleResource true
            createPattern argumentName []
        ] true
    ]
    
    let doc =
        PreXmlDoc.Create([ description ]) |> Some
    
    let updateCrosExpression setAssignmentExpression =
        let setExpression =
            Expr.set($"cros.{operationName}", setAssignmentExpression)
        
        let lambdaExpression =
            Expr.sequential([
                setExpression
                Expr.ident("cros")
            ])
        
        let listConsLambdaFirstExpression =
            Expr.lambda([
                SimplePat.typed("cros", "CustomResourceOptions")
            ], lambdaExpression)
        
        let listConsExpressions =
            Expr.paren(Expr.tuple(Expr.paren(listConsLambdaFirstExpression), Expr.ident("cros")))
        
        Expr.app(Expr.longIdent("List.Cons"), listConsExpressions)
    
    let expression =
        Expr.tuple(Expr.ident("name"),
                   Expr.ident("args"),
                   updateCrosExpression setAssignmentExpression)
    
    createMember'' doc operationName patterns attributes expression