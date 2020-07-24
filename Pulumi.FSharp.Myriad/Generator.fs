module Pulumi.FSharp.Myriad

open AstHelpers
open AstModules
open FSharp.Compiler.SyntaxTree
open Myriad.Core

let private pulumiSchemaUrl (provider : string) version =
    "https://raw.githubusercontent.com/pulumi/pulumi-"+
    provider.ToLower() +
    "/v" +
    version +
    "/provider/cmd/pulumi-resource-" +
    provider.ToLower() +
    "/schema.json"

#nowarn "25"

[<MyriadGenerator("Pulumi.FSharp")>]
type PulumiFSharpGenerator() =
    interface IMyriadGenerator with
        member _.Generate(namespace', fileInput) =
            let (ParsedInput.ImplFile(implFileInput)) = fileInput
            let (ParsedImplFileInput(_,_,_,_,_,[module'],_)) = implFileInput
            let (SynModuleOrNamespace(ident::_,_,_,let'::_,_,_,_,_)) = module'
            let (SynModuleDecl.Let(_,binding::_,_)) = let'
            let (SynBinding.Binding(_,_,_,_,_,_,_,_,_,expr,_,_)) = binding
            let (SynExpr.Const(const',_)) = expr
            let (SynConst.String(version,_)) = const'
            let provider = ident.idText            
            let url = pulumiSchemaUrl provider version
            
            Namespace.namespace'(namespace', [
                yield  Module.open'("Pulumi.FSharp")
                
                yield! createPulumiModules url provider
            ])