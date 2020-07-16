module AstRun

open AstMember
open AstOperations

let createRun =
    createMember "Run" [argsTuple true] []