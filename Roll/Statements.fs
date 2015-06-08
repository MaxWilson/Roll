module Statements

type RollSpec = Roll of dice : int * size : int * plus : int
                | AtLeast of roll : RollSpec * target : int
                | Max of rolls: RollSpec list
                | Min of rolls: RollSpec list
                | Sum of rolls: RollSpec list
                | Repeat of count : int * rolls: RollSpec

type Command = QuitCommand | RollCommand of RollSpec

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


