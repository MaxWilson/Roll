module Program
open System
open Statements
open Microsoft.FSharp.Text.Lexing
open System.Collections.Generic

// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let rec printTokens stream =
    match Lexer.tokenstream stream with
    | Parser.EOF -> ()
    | token -> printf "(%A) " token
               printTokens stream

let prompt msg processor = 
    printf "%s: " msg
    let input = ((Console.ReadLine()).Trim())
    try
        Some (processor input)
    with        
        _ -> 
            try
                input |> LexBuffer<char>.FromString |> printTokens
            with _ -> printfn "Could not tokenize"
            None

let Parse input =     
    let parsed = input |> LexBuffer<char>.FromString |> Parser.start Lexer.tokenstream 
    printfn "%A" parsed
    parsed

[<EntryPoint>]
let main argv = 
    let vals = Dictionary<string, Dictionary<string, string>>()
    let defaultOwner = ref (Dictionary<string, string>())
    let getObject owner = match owner with
                            | None -> !defaultOwner
                            | Some(ownerName) -> 
                                    match vals.TryGetValue(ownerName) with
                                            | true, v -> v
                                            | false, _ -> !defaultOwner
    let rec loop() =
        match prompt "Roll" (Parse) with
        | Some(Statements.QuitCommand) -> ()
        | Some(Statements.RollCommand(rolls)) ->
            Roller.Resolve rolls (printfn "%s") (printfn "%d crits")
            loop()
        | Some(Statements.SetValue(owner, property, value)) 
            ->
                let ownerObject = getObject owner                    
                ownerObject.[property] <- value
                loop()
        | Some(Statements.SetContext(name))
            ->
                let ownerObject = getObject (Some name)
                defaultOwner := ownerObject
                loop()
        | Some(Statements.PrintValues(name))
            -> 
                let print (name : string option) (dict : Dictionary<string, string>) =
                    match name with
                    | Some(name) ->
                        printfn "%s:" name
                    | None -> ()
                    for x in dict do
                        printfn "%s: %s" x.Key x.Value
                if name = Some("all") then
                    for x in vals do
                        print (Some x.Key) x.Value
                else
                    let x = getObject name
                    print name x
                loop()                        
        | None ->
            printfn "Sorry, I couldn't understand that"            
            loop()
    loop()
    0 // return an integer exit code

