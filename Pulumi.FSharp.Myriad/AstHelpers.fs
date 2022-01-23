module AstHelpers

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FsAst
open Core

type SimplePat =
    /// str
    static member id(str) =
        SynSimplePat.Id(Ident.Create(str), None, false, false, false, range.Zero)
        
    static member typed(str, type' : string) =
        SynSimplePat.Typed(SimplePat.id(str),
                           SynType.CreateLongIdent(type'),
                           range.Zero)

type Pat =
    static member tuple(left, right) =
        SynPatRcd.CreateTuple([
            SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(left), [])
            SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(right), [])
        ]).FromRcd
    
    static member tuple(one, two, three) =
        SynPatRcd.CreateTuple([
            SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(one), [])
            SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(two), [])
            SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(three), [])
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
    /// str
    static member ident(str) =
        SynExpr.CreateIdent(Ident.Create(str))
        
    /// str.str
    static member longIdent(str) =
        SynExpr.CreateLongIdent(LongIdentWithDots.CreateString(str))
        
    /// (exp)
    static member paren(exp) =
        SynExpr.CreateParen(exp)
        
    /// ()
    static member unit =
        SynExpr.CreateUnit
        
    /// null
    static member null' =
        SynExpr.CreateNull
        
    /// (left, right)
    static member tuple(left, right) =
        SynExpr.CreateTuple([
            left
            right
        ])
        
    /// (left, right)
    static member tuple(one, two, three) =
        SynExpr.CreateTuple([
            one
            two
            three
        ])
        
    /// (a, b, c, ...)
    static member tuple(exps) =
        SynExpr.CreateTuple(exps)
        
    /// [ ... ]
    static member list(exps) =
        SynExpr.ArrayOrList(
            false,
            exps,
            range.Zero
        )
    
    /// [ ... ]
    static member list(idents) =
        SynExpr.ArrayOrList(
            false,
            idents |> List.map Expr.ident,
            range.Zero
        )
    
    /// exp1
    /// exp2
    /// ...
    static member sequential(exps) =
        SynExpr.CreateSequential(exps)
        
    /// func arg
    static member app(func, arg) =
        SynExpr.CreateApp(func, arg)
        
    /// func arg1 arg2 ...
    static member app(func : SynExpr, args : SynExpr list) =
        match args with
        | [ ]      -> Expr.app(func, Expr.unit)
        | [x]      -> Expr.app(func, x)
        |  x :: xs -> Expr.app(Expr.app(func, x), xs)
        
    /// func arg1 arg2 ...
    static member app(func : string, args : SynExpr list) =
        Expr.app((Expr.longIdent(func) : SynExpr), args)
    
    /// func arg
    static member app(func : string, arg : string) =
        Expr.app(Expr.longIdent(func), Expr.ident(arg))
    
    /// func arg
    static member app(func : string, arg : SynExpr) =
        Expr.app(Expr.longIdent(func), arg)
            
    /// func (arg1, arg2, ...)
    static member appTuple(func : string, args) =
        Expr.app(func,
                 Expr.paren(Expr.tuple(args |> List.map Expr.ident)))
    
    static member match'(expr, clauses) =
        SynExpr.CreateMatch(expr, clauses)

    /// value
    static member const'(value) =
        SynExpr.CreateConstString(value)
    
    /// failwith msg
    static member failwith(msg) =
        Expr.app("failwith", Expr.const'(msg))
        
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
        
    static member set(identString, exp) =
        SynExpr.Set (Expr.longIdent(identString),
                     exp,
                     range.Zero)

    static member methodCall(identString, exps) =
        SynExpr.CreateInstanceMethodCall(LongIdentWithDots.CreateString(identString),
                                         Expr.paren(Expr.tuple(exps)))
    
    static member lambda(args, exp) =
        SynExpr.Lambda(false,
                       true,
                       SynSimplePats.SimplePats(args, range.Zero),
                       exp,
                       None,
                       range.Zero)
    
    static member lambda(args : string list, exp) =
        match args with
        | [ ]      -> Expr.lambda(List.empty<SynSimplePat>, exp)
        | [x]      -> Expr.lambda([ SimplePat.id(x) ], exp)
        |  x :: xs -> Expr.lambda([ SimplePat.id(x) ], Expr.lambda(xs, exp))

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