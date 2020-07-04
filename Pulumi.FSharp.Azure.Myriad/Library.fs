module Pulumi.FSharp.Azure.Myriad

open System
open FSharp.Compiler.Range
open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core

[<RequireQualifiedAccess>]
module Generator =
    type PulumiAttribute() =
        inherit Attribute()

let private createModule name content =
    let componentInfo = SynComponentInfoRcd.Create [ Ident.Create name ]
    SynModuleDecl.CreateNestedModule(componentInfo, content)

let private createNamespace name content =
    { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong name)
                with Declarations = content }

let private createAttribute name =
    SynAttributeList.Create(SynAttribute.Create(name))

let private createAttributeWithArg (name : string) (arg : string) =
    let o : SynAttribute = { TypeName = LongIdentWithDots.CreateString(name)
                             ArgExpr = SynExpr.CreateParen(SynExpr.CreateConstString(arg))
                             Target = None
                             AppliesToGetterAndSetter = false
                             Range = range.Zero }
    SynAttributeList.Create(o)
    
let private createOuterModule name content =
    { SynModuleOrNamespaceRcd.CreateModule(Ident.CreateLong name)
          with Declarations = content
               Attributes = [ createAttribute "AutoOpen" ] }

let private createOpen namespaceOrModule =
    LongIdentWithDots.CreateString(namespaceOrModule) |>
    SynModuleDecl.CreateOpen

let private inheritType name =
    SynMemberDefn.ImplicitInherit (SynType.Create name, SynExpr.CreateUnit, None, range.Zero)

let private implicitCtor () =
    SynMemberDefn.CreateImplicitCtor()

let private createPattern name args =
    SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(name), args)
    
let private createPatternTyped name args (typeName : string) =
    SynPatRcd.CreateTyped(SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(name), args),
                          SynType.CreateLongIdent(typeName))

let private createMember name args attrs expr =
    let letBinding =
        { SynBindingRcd.Null with
              Pattern = createPattern ("_." + name) args
              Expr = expr
              Attributes = attrs }

    SynMemberDefn.CreateMember(letBinding)
    
let private createYield args =
    [
        LongIdentWithDots.CreateString("AzureResource.Zero") |> SynExpr.CreateLongIdent
        args
    ] |>
    SynExpr.CreateTuple |>
    createMember "Yield" [SynPatRcd.CreateWild] []
    
let private createTuple items withParen =
    if withParen then
        SynPatRcd.CreateParen(SynPatRcd.CreateTuple(items))
    else
        SynPatRcd.CreateTuple(items)
    
let private argsTuple withParen typeName =
    createTuple [ createPattern "cargs" []
                  createPatternTyped "args" [] typeName ] withParen
    
let private createRun typeName =
    createMember "Run" [argsTuple true typeName] []

let private (|FirstLetter|) (p:string) =
    p.[0], (p.Substring(1))
    
let private toSnakeCase value =
    let (FirstLetter(x, xs)) =
        value
    
    sprintf "%c%s" (Char.ToLower(x)) xs
    
let private createOperation name typeName =
    let snakeCaseName =
        toSnakeCase name
    
    let attribute =
        createAttributeWithArg "CustomOperation" snakeCaseName
    
    createMember name [createTuple [ argsTuple true typeName; createPattern snakeCaseName [] ] true] [ attribute ]

let private createInstance name args =
    let identifier =
        LongIdentWithDots.CreateString name |>
        SynExpr.CreateLongIdent
        
    SynExpr.CreateApp(identifier, args)
    
let private createOperationFor name argsType tupleArgs =
    let setExpr =
        SynExpr.Set (SynExpr.CreateLongIdent(LongIdentWithDots.CreateString("args." + name)),
                     SynExpr.CreateIdentString(name |> toSnakeCase),
                     range.Zero)
        
    let expr =
        SynExpr.CreateSequential([
            setExpr
            SynExpr.CreateTuple(tupleArgs (SynExpr.CreateIdentString("cargs")))
        ])        
        
    createOperation name argsType expr
    
let private createAzureBuilderClass name =
    let typeName =
        name + "Builder" |>
        Ident.CreateLong
        
    let tupleArgs fst =
        [fst
         SynExpr.CreateIdentString("args")]
       
    let runArgs =
        SynExpr.CreateParenedTuple(tupleArgs (SynExpr.CreateLongIdent(LongIdentWithDots.CreateString ("cargs.Name"))))
        
    let argsType =
        name + "Args"
        
    SynModuleDecl.CreateType(SynComponentInfoRcd.Create(typeName),
                             [
                                 implicitCtor ()
                                 inheritType "AzureResource"
                                 
                                 createYield (createInstance argsType SynExpr.CreateUnit)
                                 createRun argsType (createInstance name runArgs)
                                 createOperationFor "Location" argsType tupleArgs
                             ])

let private createLet name expr =
    SynModuleDecl.CreateLet
        [ { SynBindingRcd.Let with
                Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString name, [])
                Expr = expr } ]

[<MyriadGenerator("example1")>]
type Example1Gen() =
    interface IMyriadGenerator with
        member __.Generate(_, _) =
            //let t =
            //    Assembly.GetAssembly(typeof<Pulumi.Azure.Config>)
            
            let typeName =
                "VirtualMachine"
            
            createOuterModule "Pulumi.FSharp.Azure.VirtualMachine" [
                 createOpen "Pulumi"
                 createOpen "Pulumi.FSharp.Azure.Core"
                 createOpen "Pulumi.Azure.Core"
                 createOpen "Pulumi.FSharp"
                 createOpen "Pulumi.Azure.Compute"
                 
                 createAzureBuilderClass typeName
                 
                 createLet (toSnakeCase typeName) (createInstance (typeName + "Builder") SynExpr.CreateUnit)
            ]