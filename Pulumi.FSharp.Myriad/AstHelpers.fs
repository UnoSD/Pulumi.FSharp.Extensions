module AstHelpers

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FsAst
open Core

type Pat =
    static member tuple(left, right) =
        SynPatRcd.CreateTuple([
            SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(left), [])
            SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(right), [])
        ]).FromRcd
    
    static member tuple(left : SynPat, right : SynPat) =
        SynPatRcd.CreateTuple([
            left.ToRcd
            right.ToRcd
        ]).FromRcd
    
    static member paren(pat : SynPat) =
        SynPatRcd.CreateParen(pat.ToRcd)
                 .FromRcd
        
    static member ident(str) =
        SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(str), [])
                 .FromRcd

    static member null' =
        SynPat.Null(range.Zero)

    static member wild =
        SynPat.Wild(range.Zero)
        
    static member typed(name, typeName : string) =
        {
            Pattern = Pat.ident(name).ToRcd
            Type = SynType.CreateLongIdent(typeName)
            Range = range.Zero
        } |>
        SynPatRcd.Typed |>
        SynPatRcd.CreateParen |>
        (fun x -> x.FromRcd)

type Expr =
    static member ident(str) =
        SynExpr.CreateLongIdent(LongIdentWithDots.CreateString(str))
        
    static member paren(exp) =
        SynExpr.CreateParen(exp)
        
    static member unit =
        SynExpr.CreateUnit
        
    static member null' =
        SynExpr.CreateNull
        
    static member tuple(left, right) =
        SynExpr.CreateTuple([
            left
            right
        ])
        
    static member tuple(exps) =
        SynExpr.CreateTuple(exps)
        
    static member list(exps) =
        SynExpr.ArrayOrList(
            false,
            exps,
            range.Zero
        )
    
    static member list(idents) =
        SynExpr.ArrayOrList(
            false,
            idents |> List.map Expr.ident,
            range.Zero
        )
     
    static member sequential(exps) =
        SynExpr.CreateSequential(exps)
        
    static member func(name, exp) =
        SynExpr.CreateApp(Expr.longIdent(name),
                          exp)

    static member func(name : string) =
        Expr.func(name, Expr.unit)
    
    static member funcTuple(name : string, exps) =
        Expr.func(name,
                  Expr.paren(Expr.tuple(exps)))
        
    static member funcTuple(name : string, names) =
        Expr.func(name,
                  Expr.paren(Expr.tuple(names |> List.map Expr.ident)))

    static member func(name : string, args : string list) =
        Expr.func(Expr.longIdent(name), args |> List.map Expr.ident)

    static member func(name : SynExpr, arg : SynExpr) =
        SynExpr.CreateApp(name, arg)
    
    static member func(name : SynExpr, args : SynExpr list) =
        match args with
        | [x]     -> Expr.func(name, x)
        | x :: xs -> Expr.func(name, Expr.func(x, xs))
        | []      -> failwith "Empty arguments"
        
    static member func(name : string, args : SynExpr list) =
        Expr.func(Expr.longIdent(name), args)
            
    static member func(name, arg : string) =
        Expr.func(name, [arg])
        
    static member match'(expr, clauses) =
        SynExpr.CreateMatch(expr, clauses)

    static member const'(value) =
        SynExpr.CreateConstString(value)
    
    static member failwith(msg) =
        Expr.func("failwith", Expr.const'(msg))
        
    static member let'(name, args : SynPat list, exp) =
        SynExpr.LetOrUse(
            false,
            false,
            [
                { SynBindingRcd.Let with
                     Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(name),
                                                         (args |> List.map (fun arg -> arg.ToRcd)))
                     Expr = exp }.FromRcd
            ],
            Expr.unit,
            range.Zero)
        
    static member let'(name, args, exp) =
        SynExpr.LetOrUse(
            false,
            false,
            [
                { SynBindingRcd.Let with
                     Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(name),
                                                         (args |> List.map (Pat.ident >> (fun x -> x.ToRcd))))
                     Expr = exp }.FromRcd
            ],
            Expr.unit,
            range.Zero)
      
    static member longIdent(identString) =
        identString |>
        LongIdentWithDots.CreateString |>
        SynExpr.CreateLongIdent
        
    static member set(identString, exp) =
        SynExpr.Set (Expr.longIdent(identString),
                     exp,
                     range.Zero)

    static member methodCall(identString, exps) =
        SynExpr.CreateInstanceMethodCall(LongIdentWithDots.CreateString(identString),
                                         Expr.paren(Expr.tuple(exps)))

type Match =
    static member clause(pat, expr) =
        SynMatchClause.Clause(pat,
                              None,
                              expr,
                              range.Zero,
                              DebugPointForTarget.No)

type Namespace =
    static member namespace'(name, content) =
        { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong name)
                with Declarations = content }
        
type Attribute =
    static member attribute(name) =
        SynAttributeList.Create(SynAttribute.Create(name))
        
type Module =
    static member module'(name, content, attributes) =
        let componentInfo =
            { SynComponentInfoRcd.Create [ Ident.Create name ] with 
                  Attributes = attributes }
        SynModuleDecl.CreateNestedModule(componentInfo, content)
        
    static member module'(name, content) =
        Module.module'(name, content, [])
        
    static member autoOpenModule(name, content) =
        Module.module'(name, content, [ Attribute.attribute("AutoOpen") ])
        
    static member open'(namespaceOrModule) =
        SynOpenDeclTarget.ModuleOrNamespace(LongIdentWithDots.CreateString(namespaceOrModule).Lid, range.Zero) |>
        SynModuleDecl.CreateOpen
        
    static member type'(name, content) =
        SynModuleDecl.CreateType(SynComponentInfoRcd.Create(Ident.CreateLong(name)),
                                 content)
        
type Type =
    static member ctor() =
        SynMemberDefn.CreateImplicitCtor()