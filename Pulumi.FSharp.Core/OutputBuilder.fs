module Pulumi.FSharp.Output

open Pulumi
open System.Threading.Tasks

type OutputBuilder internal (isSecret) =
    let create (value : 'a) =
        match isSecret with
        | true  -> Output.CreateSecret<'a>(value)
        | false -> Output.Create<'a>(value)
    
    member __.Return (x : 'a) =
        create x
    
    member __.Bind (comp : Output<'a>, func : 'a -> Output<'b>) =
        comp.Apply<'b>(func)
      
    member this.Bind (comp : Task<'a>, func : 'a -> Output<'b>) =
        this.Bind((match isSecret with
                   | false -> Output.Create<'a>(comp)
                   | true  -> Output.CreateSecret<'a>(comp)), func)
    
    member __.ReturnFrom (v : Output<_>) =
        v

let output = OutputBuilder(isSecret = false)
let secretOutput = OutputBuilder(isSecret = true)