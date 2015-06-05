module Parser

open Rolls
open System.Text.RegularExpressions

type Command = QuitCommand | RollCommand of RollSpec

let ParseDice input =
    let cmd = Regex.Match(input, "")
    Rolls.Roll(1, 6, 1)

let Parse line =
    match (line : string).Trim().ToLowerInvariant() with
    | "quit" | "q" -> QuitCommand
    | input -> RollCommand(ParseDice input)

    