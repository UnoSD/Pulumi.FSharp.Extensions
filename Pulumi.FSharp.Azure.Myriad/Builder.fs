module AstBuilder

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open AstOperations
open AstInstance
open AstHelpers
open AstMember
open AstYield
open AstRun
open FsAst
open Core

let private implicitCtor () =
    SynMemberDefn.CreateImplicitCtor()

let createAzureBuilderClass name props =
    let typeName =
        name + "Builder" |>
        Ident.CreateLong
        
    let argsType =
        name + "Args"
       
    let func =
        "List.fold"
       
    //let lambdaArg =
    //    SynSimplePats.SimplePats([ SynSimplePat.CreateTyped(Ident.Create("args"), SynType.Bool())
    //                               SynSimplePat.CreateTyped(Ident.Create("f"), SynType.Bool()) ], range.Zero)
        
    //let lambdaArg =
    //    SynSimplePats.SimplePats([], range.Zero)
    
    let arg name =
        SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(name), [])
    
    let letExpr =
        SynExpr.CreateApp(SynExpr.CreateIdentString("f"), SynExpr.CreateIdentString("args"))
    
    let letFunc =
        SynExpr.LetOrUse(false,
                         false,
                         [
                             { SynBindingRcd.Let with
                                  Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString("func"),
                                                                      [ arg "args"; arg "f" ])
                                  Expr = letExpr }.FromRcd
                         ],
                         SynExpr.CreateUnit,
                         range.Zero)
        
    let arg1 =
        SynExpr.CreateLongIdent(LongIdentWithDots.CreateString("func"))
    
    let arg2 =
        SynExpr.CreateParen(createInstance argsType SynExpr.CreateUnit)
        
    let arg3 =
        SynExpr.CreateLongIdent(LongIdentWithDots.CreateString("args"))
    
    let apply =
        SynExpr.CreateApp(SynExpr.CreateLongIdent(LongIdentWithDots.CreateString(func)),
                                                  SynExpr.CreateApp(arg1, SynExpr.CreateApp(arg2, arg3)))
       
    let runArgs =
        SynExpr.CreateParenedTuple([
            SynExpr.CreateLongIdent(LongIdentWithDots.CreateString ("name"))
            SynExpr.CreateParen(apply)
        ])
        
    let listCons =
        Expr.funcTuple("List.Cons", [ "apply"; "args" ])
        
    // Infix: apply :: args, but produces: apply ``::`` args
    //let listCons =
    //    SynExpr.CreateApp(SynExpr.CreateAppInfix(SynExpr.CreateLongIdent(LongIdentWithDots.CreateString("::")),
    //                                             SynExpr.CreateIdentString("apply")),
    //                      SynExpr.CreateIdentString("args"))
        
    let opTupleArgs fst =
        [fst; listCons]
        
    let operations =
        props |>
        Array.collect (fun (prop, t) -> createOperationsFor (prop |> toPascalCase) t argsType opTupleArgs) |>
        List.ofArray
    
    let newNameExpr =
        Expr.tuple(Expr.ident("newName"),
                   Expr.ident("args"))
    
    let runReturnExpr =
        Expr.sequential([
            letFunc
            createInstance name runArgs
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
        SynExpr.CreateInstanceMethodCall(LongIdentWithDots.CreateString("this.Combine"),
                                         Expr.paren(Expr.tuple(Expr.ident("args"), Expr.func("delayedArgs", Expr.unit))))
    
    let createFor() =
        createMember' "this" "For" [forArgs.ToRcd] [] forExpr
     
    let createDelay() =
        createMember "Delay" [Pat.ident("f").ToRcd] [] (Expr.func("f"))
    
    let createZero() =
        createMember "Zero" [] [] (Expr.unit)
    
    SynModuleDecl.CreateType(SynComponentInfoRcd.Create(typeName),
                             [
                                 implicitCtor ()
                                 
                                 createYield yieldReturnExpr
                                 createRun runReturnExpr
                                 createCombine()
                                 createFor()
                                 createDelay()
                                 createZero()
                                 createNameOperation newNameExpr
                             ] @ operations)