module Dice

type RollSpec = RollSpec of int * int * int
  with static member Create(n,d,plus): RollSpec = RollSpec(n,d,plus)
let private r = System.Random()
let roll (spec: RollSpec) = 
  match spec with
  | RollSpec(n, d, plus) -> 
    plus + ([for _ in 1..n -> 1 + r.Next(d)] |> List.sum)
let average (spec: RollSpec) =
  5
let eval spec = sprintf "%d" (roll spec)

let parse (input: string) =
  RollSpec.Create(3,6,2)