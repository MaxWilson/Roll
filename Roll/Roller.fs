module Roller 
open Statements

let r = System.Random()
        
let rec ResolveBase spec outputFunc = 
    match spec with
    | Roll(dice, size, plus) ->
        [for _ in 1..dice -> 1 + r.Next(size)] |> Seq.sum |> (+) plus
    | Sum rolls ->
        let rolls = [for roll in rolls -> ResolveBase roll outputFunc]
        sprintf "%A" rolls |> outputFunc
        Seq.sum rolls
    | Min rolls ->
        let rolls = [for roll in rolls -> ResolveBase roll outputFunc]
        Seq.min rolls
    | Max rolls ->
        let rolls = [for roll in rolls -> ResolveBase roll outputFunc]
        Seq.max rolls
let rec ResolveComplex spec outputFunc =
    match spec with
    | Simple(roll) -> ResolveBase roll outputFunc, 0
    | Repeat(count, roll) ->
        let rolls = [for _ in 1..count -> ResolveComplex roll outputFunc]
        sprintf "%A" rolls |> outputFunc
        Seq.sumBy fst rolls, Seq.sumBy snd rolls
    | AtLeast(roll, target, critThreshold) ->
        let roll, crits = ResolveComplex roll outputFunc
        let hit = if roll >= target then 1 else 0
        let crits = if roll >= critThreshold then crits + 1 else crits
        hit, crits
let Resolve spec output critOutput =
    let critCounter = ref 0
    let result, crits = ResolveComplex spec output
    output (sprintf "%d" result)
    if crits > 0 then
        critOutput crits
