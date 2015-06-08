module Program
open System
open Statements
open Microsoft.FSharp.Text.Lexing

// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let prompt msg processor = 
    printf "%s: " msg
    try
        Some (processor ((Console.ReadLine()).Trim()))
    with
        _ -> None

let Parse input =     
    let parsed = input |> LexBuffer<char>.FromString |> Parser.start Lexer.tokenstream 
    printfn "%A" parsed
    parsed

[<EntryPoint>]
let main argv = 
    let rec loop() =
        match prompt "Roll" (Parse) with
        | Some(Statements.QuitCommand) -> ()
        | Some(Statements.RollCommand(rolls)) ->
            printfn "%d" (Roller.Resolve rolls)
            loop()
        | None ->
            printfn "Sorry, I couldn't understand that"
            loop()
    loop()
    0 // return an integer exit code

