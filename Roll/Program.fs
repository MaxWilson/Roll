module Program
open System
open Rolls

// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let prompt msg processor = 
    printf "%s: " msg
    try
        Some (processor ((Console.ReadLine()).Trim()))
    with
        _ -> None
    
[<EntryPoint>]
let main argv = 
    let rec loop() =
        match prompt "Roll" (DiceParser.Parse) with
        | Some(DiceParser.QuitCommand) -> ()
        | Some(DiceParser.RollCommand(spec)) ->
            printfn "%d" (Roller.Resolve spec)
            loop()
        | None ->
            printfn "Sorry, I couldn't understand that"
            loop()
    loop()
    0 // return an integer exit code

