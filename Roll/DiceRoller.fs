module Rolls

type RollSpec = Roll of dice : int * size : int * plus : int
                | AtLeast of roll : RollSpec * target : int
                | Max of rolls: RollSpec list
                | Min of rolls: RollSpec list
                | Sum of rolls: RollSpec list
                | Repeat of count : int * rolls: RollSpec

let MakeSum(head, tail) = 
    match head with
    | Roll(x1, y1, z1) -> 
        match tail with
        // Code smell: various messy ways of making sums
        | Roll(x2, y2, z2) when x1 = 0 || x2 = 0 ->
            Roll(max x1 x2, max y1 y2, z1 + z2)
        | Roll(x2, y2, z2) when y1 = y2 ->
            Roll(x1 + x2, y1, z1 + z2)
        | Sum (lst : RollSpec list) -> Sum (List.append [head] lst)
        | _ -> Sum [head; tail]
    | _ -> Sum [head; tail]
let MakeSubtract(head, tail) = 
    match head with
    | Roll(x, y, z) -> 
        match tail with
        | Roll(0, 0, n) ->
            Roll(x, y, z - n)
        | _ -> failwith "Only simple subtraction supported in MakeSubtract"
    | _ -> failwith "Only simple subtraction supported in MakeSubtract"

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
    let Resolve spec =
        ResolveBase spec (printfn "%s")

