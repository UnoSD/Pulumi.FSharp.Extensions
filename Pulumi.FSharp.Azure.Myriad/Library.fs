module Pulumi.FSharp.Azure.Myriad

open System
open FSharp.Data
open FSharp.Compiler.Range
open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core

[<Literal>]
let private pulumiSchemaUrl =
    "https://raw.githubusercontent.com/pulumi/pulumi-azure/master/provider/cmd/pulumi-resource-azure/schema.json"

type private PulumiProvider =
    JsonProvider<pulumiSchemaUrl>

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
    
let private changeInitial change value =
    let (FirstLetter(x, xs)) =
        value
    
    sprintf "%c%s" (change x) xs

let private toSnakeCase =
    changeInitial Char.ToLower
    
let private toPascalCase =
    changeInitial Char.ToUpper
    
let private createOperation name typeName hasAttribute =
    let snakeCaseName =
        toSnakeCase name
    
    let coName =
        match snakeCaseName with
        | "resourceGroupName" -> "resourceGroup"
        | x -> x
    
    let attributes =
        if hasAttribute then
            [ createAttributeWithArg "CustomOperation" coName ]
        else
            []
    
    createMember name [createTuple [ argsTuple true typeName; createPattern snakeCaseName [] ] true] attributes

let private createInstance name args =
    let identifier =
        LongIdentWithDots.CreateString name |>
        SynExpr.CreateLongIdent
        
    SynExpr.CreateApp(identifier, args)
    
let private createOperationsFor name pType argsType tupleArgs =
    let setRights =
        match pType with
        | "string"
        | "boolean" -> [ SynExpr.CreateIdentString("input"); SynExpr.CreateIdentString("io") ]
        | "array" -> [ SynExpr.CreateIdentString("inputList") ]
        | "object" -> [ SynExpr.CreateIdentString("inputMap") ]
        // What to do here?
        | "complex" -> [ SynExpr.CreateIdentString("input") ]
    
    let setExpr setRight =
        SynExpr.Set (SynExpr.CreateLongIdent(LongIdentWithDots.CreateString("args." + name)),
                     SynExpr.CreateApp(setRight),
                     range.Zero)
        
    let expr setExpr =
        SynExpr.CreateSequential([
            setExpr
            SynExpr.CreateTuple(tupleArgs (SynExpr.CreateIdentString("cargs")))
        ])        
        
    setRights |>
    List.map (fun sr -> sr, SynExpr.CreateIdentString(name |> toSnakeCase)) |>
    List.map (setExpr >> expr) |>
    List.mapi (fun i e -> createOperation name argsType (i = 0) e) |>
    Array.ofList
    
let private createAzureBuilderClass name props =
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
        
    let operations =
        props |>
        Array.collect (fun (prop, t) -> createOperationsFor (prop |> toPascalCase) t argsType tupleArgs) |>
        List.ofArray
        
    SynModuleDecl.CreateType(SynComponentInfoRcd.Create(typeName),
                             [
                                 implicitCtor ()
                                 //inheritType "AzureResource"
                                 
                                 createYield (createInstance argsType SynExpr.CreateUnit)
                                 createRun argsType (createInstance name runArgs)
                             ] @ operations)

let private createLet name expr =
    SynModuleDecl.CreateLet
        [ { SynBindingRcd.Let with
                Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString name, [])
                Expr = expr } ]
                
#nowarn "25"

let private createType (provider : PulumiProvider.Root) (fqType : string, jValue : JsonValue) =
    let getComplexType (v : JsonValue) =
        provider.Types.JsonValue.Properties() |>
        Array.tryFind (fun (t, _) -> ("#/types/" + t) = v.AsString()) |>
        ignore
        "complex"

    let [| fullProvider; fullType |] = fqType.Split("/")
    let [| provider; category |] = fullProvider.Split(':')
    let [| resourceType; _(*subtype*) |] = fullType.Split(':')
    let ns = sprintf "Pulumi.%s.%s" (toPascalCase provider) (toPascalCase category)
    let typeName = toPascalCase resourceType
    let properties = jValue.GetProperty("properties").Properties()
    
    let nameAndType (name, jValue : JsonValue) =
        let pType =
            jValue.Properties() |>
            Array.choose (fun (p, v) -> match p with
                                        | "type" -> v.AsString() |> Some // Array type has also "items"
                                        | "$ref" -> getComplexType v |> Some
                                        (*| "description"*)
                                        | _ -> None) |>
            Array.head
        
        (name, pType)
    
    createOuterModule ("Pulumi.FSharp.Azure." + typeName) [
         createOpen "Pulumi.FSharp.Azure.Core"
         createOpen "Pulumi.FSharp"
         createOpen ns
         
         createAzureBuilderClass typeName (properties |> Array.map (nameAndType))
         
         createLet (toSnakeCase typeName) (createInstance (typeName + "Builder") SynExpr.CreateUnit)
    ]

[<MyriadGenerator("example1")>]
type Example1Gen() =
    interface IMyriadGenerator with
        member __.Generate(_, _) =
            let provider = PulumiProvider.GetSample()
            
            provider.Resources.JsonValue.Properties() |>
            Array.filter (fun (r, _) -> r = "azure:compute/virtualMachine:VirtualMachine") |> // Remove
            Array.map (createType provider) |>
            Array.head // Remove