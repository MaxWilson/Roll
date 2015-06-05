namespace Rolls

type RollSpec = Roll of dice : int * size : int * plus : int
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
        | _ -> failwith "Not implemented"
    let Resolve spec =
        ResolveBase spec (printfn "%s")

