module DiceParser

open Rolls
open System.Text.RegularExpressions
open Microsoft.FSharp.Text.Lexing

type Command = QuitCommand | RollCommand of RollSpec

let ParseDice input =
    let lexbuf = LexBuffer<char>.FromString input
    let parsed = Parser.start Lexer.tokenstream lexbuf
    printfn "%A" parsed
    parsed

let Parse line =
    match (line : string).Trim().ToLowerInvariant() with
    | "quit" | "q" -> QuitCommand
    | input -> RollCommand(ParseDice input)

    