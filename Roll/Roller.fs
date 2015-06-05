namespace Rolls

type RollSpec = Roll of dice : int * size : int * plus : int
                | Max of rolls: RollSpec[]
                | Min of rolls: RollSpec[]
                | Sum of rolls: RollSpec[]

module Roller =
    let r = System.Random()
    let Resolve spec = 
        match spec with
        | Roll(dice, size, plus) ->
            [for _ in 1..dice -> 1 + r.Next(size)] |> Seq.sum |> (+) plus
        | _ -> failwith "Not implemented"

