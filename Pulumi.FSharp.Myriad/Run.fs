module AstRun

open AstMember
open AstOperations

let createRunResource =
    createMember "Run" (argsTupleResource true) []
    
let createRunType =
    createMember "Run" (argsTupleType true) []