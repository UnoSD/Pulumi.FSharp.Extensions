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
            let (SynExpr.AnonRecd(_,_,kvps,_)) = expr
            let getValue = function | SynConst.String(str,_) -> str | SynConst.Bool(b) -> b.ToString() | x -> x.ToString()
            let config = kvps |> List.map (fun (key, SynExpr.Const(const', _)) -> (key.idText, getValue const')) |> Map.ofList
            let provider = ident.idText            
            let url = pulumiSchemaUrl provider config.["Version"]
            
            Namespace.namespace'(namespace', [
                yield  Module.open'("Pulumi.FSharp")
                
                yield! createPulumiModules url provider
            ])