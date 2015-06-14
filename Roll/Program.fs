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
    #if DEBUG
    printfn "%A" parsed
    #endif
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
                                            | false, _ ->
                                                let newOwner = Dictionary<string, string>()
                                                vals.[ownerName] <- newOwner
                                                newOwner 
    let rec loop() =
        match prompt "Roll" (Parse) with
        | Some(Statements.QuitCommand) -> Environment.Exit 0
        | Some(Statements.RollCommand(rolls)) ->
            Roller.Resolve rolls (printfn "%s") (printfn "%d crits")
        | Some(Statements.SetValue(owner, property, value)) 
            ->
                let ownerObject = getObject owner   
                ownerObject.[property] <- value
        | Some(Statements.SetContext(name))
            ->
                let ownerObject = getObject (Some name)
                defaultOwner := ownerObject
        | Some(Statements.AddValue(owner, property, value))
            ->
                let ownerObject = getObject owner
                let oldValue = match ownerObject.TryGetValue(property) with
                               | true, v -> v
                               | false, _ -> "0"
                ownerObject.[property] <- 
                        match System.Int32.TryParse(oldValue) with
                            | true, v -> v + value
                            | false, _ -> value
                        |> sprintf "%d"
        | Some(Statements.PrintValues(name))
            -> 
                let print (name : string option) (dict : Dictionary<string, string>) =
                    match name with
                    | Some(name) ->
                        printfn "%s:" name
                    | None ->
                        match vals |> Seq.tryFind (fun kv -> kv.Value = dict) with
                        | Some(kv) ->
                            printfn "%s:" kv.Key
                        | _ -> ()
                    for x in dict do
                        printfn "%s: %s" x.Key x.Value
                if name = Some("all") then
                    for x in vals do
                        print (Some x.Key) x.Value
                else
                    let x = getObject name
                    print name x
        | Some(ResolveAction(owner)) ->
            let ownerObject = getObject owner
            if ownerObject.ContainsKey("action") then
                ownerObject.["action_log"] <- 
                    if ownerObject.ContainsKey("action_log") then
                        sprintf "%s\n%s" (ownerObject.["action_log"]) (ownerObject.["action"])
                    else
                        (ownerObject.["action"])
                ownerObject.Remove("action") |> ignore
            let next = vals |> Seq.sortBy (fun x -> x.Key)
                            |> Seq.tryFind (fun x -> 
                                             let props = x.Value
                                             props.ContainsKey("action")
                                          )
            if next.IsSome then
                printfn "%s: %s" next.Value.Key (next.Value.Value.["action"])
                defaultOwner := next.Value.Value
            else
                printfn "Done with round"
        | Some(Delete(name)) ->
            vals.Remove(name) |> ignore
        | None ->
            printfn "Sorry, I couldn't understand that"            
        loop()
    loop()
    0 // return an integer exit code

