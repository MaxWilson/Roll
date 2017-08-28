module RollLib

type RollSpec = int * int * int
let r = System.Random()
let roll (spec: RollSpec) = 
  match spec with 
  | (n, d, plus) ->
    plus + ([for x in 1..n -> (1 + r.Next(d))] |> List.sum)
 
