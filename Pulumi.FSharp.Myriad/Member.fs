module AstMember

open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Core
open FSharp.Compiler.Xml
open Myriad.Core.Ast
open Myriad.Core.AstExtensions
    
let createMember' xmlDoc this name args attrs expr =
    
    //let memberFlags : MemberFlags = 
    //let b : SynBindingRcd = 
    //    {   
    //        ValData = SynValData(Some memberFlags, SynValInfo([], SynArgInfo(SynAttributes.Empty, false, None)), None)
    //        Pat = SynPat.LongIdent(LongIdentWithDots([mkId "x"; mkId "Points"], [range.Zero]), None, None, SynConstructorArgs.Pats[], None, range.Zero)
    //    }
    let memberFlag : SynMemberFlags =
        {
            IsInstance = true
            IsDispatchSlot = false
            IsOverrideOrExplicitImpl = false
            IsFinal = false
            MemberKind = SynMemberKind.Member
            Trivia = {
                MemberRange = Some Range.Zero
                OverrideRange = None
                AbstractRange = None
                StaticRange = None
                DefaultRange = None
            }
        }
    
    let argsPat =
        match args with
        | SynPat.Paren _
        | SynPat.Wild  _ -> args
        | _              -> SynPat.Paren(args, Range.Zero)
    
    SynBinding.SynBinding(None,
                          SynBindingKind.Normal,
                          false,
                          false,
                          attrs,
                          (match xmlDoc with | Some x -> x | None -> PreXmlDoc.Empty),
                          SynValData.SynValData(Some memberFlag, SynValInfo([], SynArgInfo(SynAttributes.Empty, false, None))(*SynValInfo.Empty*), None),
                          SynPat.CreateLongIdent(LongIdentWithDots.CreateString(this + "." + name), [argsPat]),
                          None,
                          expr,
                          Range.Zero,
                          DebugPointAtBinding.NoneAtInvisible,
                          { LetKeyword = Some Range.Zero
                            EqualsRange = Some Range.Zero })
    |> fun x -> SynMemberDefn.Member(x, Range.Zero)
    
let createMember'' xmlDoc =
    createMember' xmlDoc "_"
    
let createMember =
    createMember' None "_"