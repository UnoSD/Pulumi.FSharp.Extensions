module AstHelpers

open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Core
open Myriad.Core.Ast
open Myriad.Core.AstExtensions

type SimplePat =
    /// str
    static member id(str) =
        SynSimplePat.Id(Ident.Create(str), None, false, false, false, range.Zero)

    static member hashTyped(str, type': string) =
        SynSimplePat.CreateTyped(
            Ident.Create str,
            SynType.HashConstraint(SynType.CreateLongIdent(type'), range.Zero)
        )

    static member typed(str, type': string) =
        SynSimplePat.Typed(SimplePat.id (str), SynType.CreateLongIdent(type'), range.Zero)

type SynPat with

    static member CreateTuple(args: SynPat list) = SynPat.Tuple(false, args, Range.Zero)

type Pat =
    static member tuple(left, right) =
        SynPat.CreateTuple(
            [
                SynPat.CreateLongIdent(LongIdentWithDots.CreateString(left), [])
                SynPat.CreateLongIdent(LongIdentWithDots.CreateString(right), [])
            ]
        )

    static member tuple(one, two, three) =
        SynPat.CreateTuple(
            [
                SynPat.CreateLongIdent(LongIdentWithDots.CreateString(one), [])
                SynPat.CreateLongIdent(LongIdentWithDots.CreateString(two), [])
                SynPat.CreateLongIdent(LongIdentWithDots.CreateString(three), [])
            ]
        )

    static member tuple(left: SynPat, right: SynPat) =
        SynPat.CreateTuple(
            [
                left
                right
            ]
        )

    static member paren(pat: SynPat) = SynPat.CreateParen(pat)

    static member ident(str) =
        SynPat.CreateLongIdent(LongIdentWithDots.CreateString(str), [])

    static member null' = SynPat.Null(range.Zero)

    static member wild = SynPat.Wild(range.Zero)

    static member typed(name, typeName: string) =
        SynPat.Typed(Pat.ident (name), SynType.CreateLongIdent(typeName), range.Zero)
        |> SynPat.CreateParen

type Expr =
    /// str
    static member ident(str) = SynExpr.CreateIdent(Ident.Create(str))

    /// str.str
    static member longIdent(str) =
        SynExpr.CreateLongIdent(LongIdentWithDots.CreateString(str))

    /// (exp)
    static member paren(exp) = SynExpr.CreateParen(exp)

    /// ()
    static member unit = SynExpr.CreateUnit

    /// null
    static member null' = SynExpr.CreateNull

    /// (left, right)
    static member tuple(left, right) =
        SynExpr.CreateTuple(
            [
                left
                right
            ]
        )

    /// (left, right)
    static member tuple(one, two, three) =
        SynExpr.CreateTuple(
            [
                one
                two
                three
            ]
        )

    /// (a, b, c, ...)
    static member tuple(exps) = SynExpr.CreateTuple(exps)

    /// [ ... ]
    static member list(exps) =
        SynExpr.ArrayOrList(false, exps, range.Zero)

    /// [ ... ]
    static member list(idents) =
        SynExpr.ArrayOrList(
            false,
            idents
            |> List.map Expr.ident,
            range.Zero
        )

    /// exp1
    /// exp2
    /// ...
    static member sequential(exps) = SynExpr.CreateSequential(exps)

    /// func arg
    static member app(func, arg) = SynExpr.CreateApp(func, arg)

    /// func arg1 arg2 ...
    static member app(func: SynExpr, args: SynExpr list) =
        match args with
        | [] -> Expr.app (func, Expr.unit)
        | [ x ] -> Expr.app (func, x)
        | x :: xs -> Expr.app (Expr.app (func, x), xs)

    /// func arg1 arg2 ...
    static member app(func: string, args: SynExpr list) =
        Expr.app ((Expr.longIdent (func): SynExpr), args)

    /// func arg
    static member app(func: string, arg: string) =
        Expr.app (Expr.longIdent (func), Expr.ident (arg))

    /// func arg
    static member app(func: string, arg: SynExpr) = Expr.app (Expr.longIdent (func), arg)

    /// func (arg1, arg2, ...)
    static member appTuple(func: string, args) =
        Expr.app (
            func,
            Expr.paren (
                Expr.tuple (
                    args
                    |> List.map Expr.ident
                )
            )
        )

    static member match'(expr, clauses) = SynExpr.CreateMatch(expr, clauses)

    /// value
    static member const'(value) = SynExpr.CreateConstString(value)

    /// failwith msg
    static member failwith(msg) =
        Expr.app ("failwith", Expr.const' (msg))

    static member let'(name, args: SynPat list, exp) =
        SynExpr.LetOrUse(
            false,
            false,
            [
                SynBinding.Let(
                    pattern = SynPat.CreateLongIdent(LongIdentWithDots.CreateString(name), args),
                    expr = exp
                )
            ],
            Expr.unit,
            range.Zero,
            { InKeyword = None }
        )

    static member let'(name, args, exp) =
        SynExpr.LetOrUse(
            false,
            false,
            [
                SynBinding.Let(
                    pattern =
                        SynPat.CreateLongIdent(
                            LongIdentWithDots.CreateString(name),
                            (args
                             |> List.map Pat.ident)
                        ),
                    expr = exp
                )
            ],
            Expr.unit,
            range.Zero,
            { InKeyword = None }
        )

    static member set(identString, exp) =
        SynExpr.Set(Expr.longIdent (identString), exp, range.Zero)

    static member methodCall(identString, exps) =
        SynExpr.CreateInstanceMethodCall(
            LongIdentWithDots.CreateString(identString),
            Expr.paren (Expr.tuple (exps))
        )

    static member lambda(args: SynSimplePat list, exp: SynExpr) =
        let mapArgs =
            function
            | SynSimplePat.Id(x, _, _, _, _, _) ->
                SynPat.CreateLongIdent(LongIdentWithDots.CreateString(x.idText), [])
            | SynSimplePat.Typed(SynSimplePat.Id(ident, _, _, _, _, _), targetType, _) ->
                SynPat.CreateParen(
                    SynPat.CreateTyped(
                        SynPat.CreateLongIdent(LongIdentWithDots.CreateString(ident.idText), []),
                        targetType
                    )
                )
            | x ->
                sprintf "%A" x
                |> failwith

        let pats =
            args
            |> List.map mapArgs

        let pat =
            match pats with
            | [] -> SynPat.CreateTuple(pats)
            | [ _ ] -> SynPat.CreateTuple(pats)
            | _ -> SynPat.CreateParen(SynPat.CreateTuple(pats))

        SynExpr.CreateLambda([ pat ], exp)

    static member lambda(args: string list, exp) =
        SynExpr.CreateLambda(
            args
            |> List.map (fun x -> SynPat.CreateLongIdent(LongIdentWithDots.CreateString(x), [])),
            exp
        )

//match args with
//| [ ]      -> Expr.lambda(List.empty<SynSimplePat>, exp)
//| [x]      -> Expr.lambda([ SimplePat.id(x) ], exp)
//|  x :: xs -> Expr.lambda([ SimplePat.id(x) ], Expr.lambda(xs, exp))

type Match =
    static member clause(pat, expr) =
        SynMatchClause.SynMatchClause(
            pat,
            None,
            expr,
            range.Zero,
            DebugPointAtTarget.No,
            {
                ArrowRange = Some range.Zero
                BarRange = Some range.Zero
            }
        )

type Namespace =
    static member namespace'(name, content) =
        SynModuleOrNamespace.CreateNamespace(Ident.CreateLong name, decls = content)

type Attribute =
    static member attribute(name) =
        SynAttributeList.Create(SynAttribute.Create(name))

type Module =
    static member module'(name, content, attributes) =
        let componentInfo =
            SynComponentInfo.Create([ Ident.Create name ], attributes = attributes)

        SynModuleDecl.NestedModule(
            componentInfo,
            false,
            content,
            false,
            Range.Zero,
            {
                EqualsRange = Some Range.Zero
                ModuleKeyword = Some Range.Zero
            }
        )

    static member module'(name, content) = Module.module' (name, content, [])

    static member autoOpenModule(name, content) =
        Module.module' (name, content, [ Attribute.attribute ("AutoOpen") ])

    static member open'(namespaceOrModule) =
        SynOpenDeclTarget.ModuleOrNamespace(
            LongIdentWithDots.CreateString(namespaceOrModule).Lid,
            range.Zero
        )
        |> SynModuleDecl.CreateOpen

    static member type'(name, content) =
        let t =
            SynTypeDefn.SynTypeDefn(
                SynComponentInfo.Create(Ident.CreateLong(name)),
                SynTypeDefnRepr.ObjectModel(
                    SynTypeDefnKind.Unspecified,
                    [ SynMemberDefn.CreateImplicitCtor() ],
                    Range.Zero
                ),
                content,
                None,
                Range.Zero,
                {
                    SynTypeDefnTrivia.TypeKeyword = Some Range.Zero
                    EqualsRange = Some Range.Zero
                    WithKeyword = None
                }
            )

        SynModuleDecl.Types([ t ], Range.Zero)

type Type =
    static member ctor() = SynMemberDefn.CreateImplicitCtor()
