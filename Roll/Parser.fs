module Parser
open Rolls

let Parse line =
    match (line : string).Trim().ToLowerInvariant() with
    | "quit" | "q" -> None
    | _ -> Some (Roll(1, 6, 1))