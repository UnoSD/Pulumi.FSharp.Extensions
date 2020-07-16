module AstModule

open FSharp.Compiler.SyntaxTree
open AstOpen
open AstBuilder
open AstLet
open Core
open AstInstance
open AstAttribute
open FsAst

let private createModule' name content =
    let componentInfo =
        { SynComponentInfoRcd.Create [ Ident.Create name ] with 
              Attributes = [ createAttribute "AutoOpen" ]  }
    SynModuleDecl.CreateNestedModule(componentInfo, content)

let createModule (ns, typeName, properties, nameAndType, serviceProvider) =
    createModule' ((serviceProvider |> toPascalCase) + typeName) [
         createOpen ns
         
         createAzureBuilderClass typeName (properties |> Array.map (nameAndType))
         
         createLet (toSnakeCase (serviceProvider + typeName)) (createInstance (typeName + "Builder") SynExpr.CreateUnit)             
    ]