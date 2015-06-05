module Parser

let Parse line =
    match (line : string).Trim().ToLowerInvariant() with
    | "quit" -> None
    | _ -> None