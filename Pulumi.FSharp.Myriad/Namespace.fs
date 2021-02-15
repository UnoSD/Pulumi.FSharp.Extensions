module AstNamespace

open AstHelpers

let createNamespace provider modules = 
    Namespace.namespace'($"Pulumi.FSharp.{provider}", [
       yield  Module.open'("Pulumi.FSharp")
    
       yield! modules
   ])