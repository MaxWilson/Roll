module Statements

type RollPrimitive = 
                | Roll of dice : int * size : int * plus : int
                | Max of rolls: RollPrimitive list
                | Min of rolls: RollPrimitive list
                | Sum of rolls: RollPrimitive list

type RollComplex =
    | Simple of roll : RollPrimitive
    | AtLeast of roll : RollComplex * target : int * highTarget : int
    | Repeat of count : int * rolls: RollComplex

type Command = 
    | QuitCommand 
    | RollCommand of RollComplex
    | SetValue of owner : string option * property : string * value : string
    | AddValue of owner : string option * property : string * value : int
    | SetContext of owner : string
    | PrintValues of owner : string option * property : string option
    | ResolveAction of owner : string option * text : string option
    | Delete of name : string * property : string option
    | Delay of name: string option
    | Load of file : string
    | Save of file: string
    | Log of name : string option * text : string

let MakeSum(head, tail) : RollPrimitive = 
    match head with
    | Roll(x1, y1, z1) -> 
        match tail with
        // Code smell: various messy ways of making sums
        | Roll(x2, y2, z2) when x1 = 0 || x2 = 0 ->
            Roll(max x1 x2, max y1 y2, z1 + z2)
        | Roll(x2, y2, z2) when y1 = y2 ->
            Roll(x1 + x2, y1, z1 + z2)
        | Sum (lst : RollPrimitive list) -> Sum (List.append [head] lst)
        | _ -> Sum [head; tail]
    | _ -> Sum [head; tail]
let MakeSubtract(head, tail) : RollPrimitive = 
    match head with
    | Roll(x, y, z) -> 
        match tail with
        | Roll(0, 0, n) ->
            Roll(x, y, z - n)
        | _ -> failwith "Only simple subtraction supported in MakeSubtract"
    | _ -> failwith "Only simple subtraction supported in MakeSubtract"


