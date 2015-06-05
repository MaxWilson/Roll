namespace Rolls

type RollSpec = Roll of dice : int * size : int * plus : int
                | Max of rolls: RollSpec list
                | Min of rolls: RollSpec list
                | Sum of rolls: RollSpec list
                | Repeat of count : int * rolls: RollSpec

module Roller =
    let r = System.Random()
    let Resolve spec = 
        match spec with
        | Roll(dice, size, plus) ->
            [for _ in 1..dice -> 1 + r.Next(size)] |> Seq.sum |> (+) plus
        | _ -> failwith "Not implemented"

