module AstRun

open AstMember
open AstOperations

let createRun nameArgName =
    createMember "Run" [(argsTuple' nameArgName) true] []