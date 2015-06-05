namespace Rolls

type RollSpec = Roll of dice : int * size : int * plus : int
                | AtLeast of roll : RollSpec * target : int
                | Max of rolls: RollSpec list
                | Min of rolls: RollSpec list
                | Sum of rolls: RollSpec list
                | Repeat of count : int * rolls: RollSpec

module Roller =
    let r = System.Random()
        
    let rec ResolveBase spec outputFunc = 
        match spec with
        | Roll(dice, size, plus) ->
            [for _ in 1..dice -> 1 + r.Next(size)] |> Seq.sum |> (+) plus
        | Repeat(count, roll) ->
            let rolls = [for _ in 1..count -> ResolveBase roll outputFunc]
            sprintf "%A" rolls |> outputFunc
            Seq.sum rolls
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
        | AtLeast(roll, target) ->
            let roll = ResolveBase roll outputFunc
            if roll >= target then 1 else 0
        | _ -> failwith "Not implemented"
    let Resolve spec =
        ResolveBase spec (printfn "%s")

