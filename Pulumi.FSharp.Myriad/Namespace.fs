module AstNamespace

open AstHelpers

let createNamespace module' = 
    Namespace.namespace'("Pulumi.FSharp", [
        Module.open'("Pulumi.FSharp")
     
        module'
   ])