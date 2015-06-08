module Roller 
open Statements

let r = System.Random()
        
let rec ResolveBase spec outputFunc critCounter = 
    match spec with
    | Roll(dice, size, plus) ->
        [for _ in 1..dice -> 1 + r.Next(size)] |> Seq.sum |> (+) plus
    | Sum rolls ->
        let rolls = [for roll in rolls -> ResolveBase roll outputFunc critCounter]
        sprintf "%A" rolls |> outputFunc
        Seq.sum rolls
    | Min rolls ->
        let rolls = [for roll in rolls -> ResolveBase roll outputFunc critCounter]
        Seq.min rolls
    | Max rolls ->
        let rolls = [for roll in rolls -> ResolveBase roll outputFunc critCounter]
        Seq.max rolls
let rec ResolveComplex spec outputFunc critCounter =
    match spec with
    | Simple(roll) -> ResolveBase roll outputFunc critCounter
    | Repeat(count, roll) ->
        let rolls = [for _ in 1..count -> ResolveComplex roll outputFunc critCounter]
        sprintf "%A" rolls |> outputFunc
        Seq.sum rolls    
    | AtLeast(roll, target, critThreshold) ->
        let roll = ResolveComplex roll outputFunc critCounter
        if roll >= critThreshold then incr critCounter
        if roll >= target then 1 else 0
let Resolve spec =
    let critCounter = ref 0
    let result = ResolveComplex spec (printfn "%s") critCounter
    if !critCounter > 0 then
        printfn "%d crits" !critCounter
    result