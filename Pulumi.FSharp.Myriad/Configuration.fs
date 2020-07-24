module AstConfiguration

open FSharp.Compiler.SyntaxTree

let getSchemaUrl (provider : string) version =
    "https://raw.githubusercontent.com/pulumi/pulumi-"+
    provider.ToLower() +
    "/v" +
    version +
    "/provider/cmd/pulumi-resource-" +
    provider.ToLower() +
    "/schema.json"

#nowarn "25"

let readConfig fileInput =
    let (ParsedInput.ImplFile(implFileInput)) = fileInput
    let (ParsedImplFileInput(_,_,_,_,_,[module'],_)) = implFileInput
    let (SynModuleOrNamespace(ident::_,_,_,let'::_,_,_,_,_)) = module'
    let (SynModuleDecl.Let(_,binding::_,_)) = let'
    let (SynBinding.Binding(_,_,_,_,_,_,_,_,_,expr,_,_)) = binding
    let (SynExpr.AnonRecd(_,_,keyValuePairs,_)) = expr
    
    let getValue = function | SynConst.String(str,_) -> str | SynConst.Bool(b) -> b.ToString() | x -> x.ToString()
    
    keyValuePairs |>
    List.map (fun (key, SynExpr.Const(const', _)) -> (key.idText, getValue const')) |>
    Map.ofList |>
    Map.add "Provider" ident.idText