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
    let getCreature owner = match owner with
                            | None -> !defaultOwner
                            | Some(ownerName) -> 
                                    match vals.TryGetValue(ownerName) with
                                            | true, v -> v
                                            | false, _ ->
                                                let newOwner = Dictionary<string, string>()
                                                vals.[ownerName] <- newOwner
                                                newOwner 
    let setNext() =
        let next = vals |> Seq.sortBy (fun x -> 
                                    // sort by initval ascending, then name
                                    match x.Value.TryGetValue("initval") with
                                    | true, v -> System.Int32.TryParse(v) |> snd |> (*)-1, x.Key
                                    | false, _ -> 0, x.Key
                                    )
                    |> Seq.tryFind (fun x -> 
                                        let props = x.Value
                                        props.ContainsKey("action")
                                    )
        if next.IsSome then
            printfn "[%s] %s: %s" (next.Value.Value.["initval"]) next.Value.Key (next.Value.Value.["action"])
            defaultOwner := next.Value.Value
        else
            defaultOwner := new Dictionary<string, string>()
            printfn "Done with round"

    let rec loop() =
        match prompt "Roll" (Parse) with
        | Some(Statements.QuitCommand) -> Environment.Exit 0
        | Some(Statements.RollCommand(rolls)) ->
            Roller.Resolve rolls (printfn "%s") (printfn "%d crits")
        | Some(Statements.SetValue(owner, property, value)) 
            ->
                let ownerObject = getCreature owner   
                ownerObject.[property] <- value
        | Some(Statements.SetContext(name))
            ->
                let ownerObject = getCreature (Some name)
                defaultOwner := ownerObject
        | Some(Statements.AddValue(owner, property, value))
            ->
                let ownerObject = getCreature owner
                let oldValue = match ownerObject.TryGetValue(property) with
                               | true, v -> v
                               | false, _ -> "0"
                ownerObject.[property] <- 
                        match System.Int32.TryParse(oldValue) with
                            | true, v -> v + value
                            | false, _ -> value
                        |> sprintf "%d"
        | Some(Statements.PrintValues(name, property))
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
                        if property.IsNone || property.Value = x.Key then
                            printfn "  %s: %s" x.Key x.Value
                if name = Some("all") then
                    for x in vals do
                        print (Some x.Key) x.Value
                else
                    let x = getCreature name
                    print name x
                printfn ""
        | Some(Delay(name)) ->
            let creature = getCreature name
            let initval = (creature.TryGetValue("initval") |> snd |> System.Int32.TryParse |> snd)
            creature.["initval"] <- (initval - 100).ToString()
            setNext()
        | Some(ResolveAction(owner)) ->
            for creature in vals do
                if creature.Value.ContainsKey("action") && (not <| creature.Value.ContainsKey("initval")) then
                    let init = 
                        match creature.Value.TryGetValue("init") with
                        | true, v -> v |> System.Int32.TryParse |> snd
                        | false, _ -> 0
                    let initval = Roller.ResolveBase (Roll(1,20, init)) (fun x -> ())
                    printfn "%s init: %d" creature.Key initval
                    creature.Value.["initval"] <- initval.ToString() 
            let ownerObject = getCreature owner
            if ownerObject.ContainsKey("action") then
                ownerObject.["action_log"] <- 
                    if ownerObject.ContainsKey("action_log") then
                        sprintf "%s\n%s" (ownerObject.["action_log"]) (ownerObject.["action"])
                    else
                        (ownerObject.["action"])
                ownerObject.Remove("action") |> ignore
            setNext()
        | Some(Delete(name)) ->
            vals.Remove(name) |> ignore
        | Some(Save(file)) ->
            let json = Newtonsoft.Json.JsonConvert.SerializeObject(vals)
            let file = if file.Contains(".json") then file else file + ".json"
            try
                System.IO.File.WriteAllText(file, json)
            with e ->
                printfn "Error: %s" e.Message
        | Some(Load(file)) ->
            let file = if file.Contains(".json") then file else file + ".json"
            try
                let json = System.IO.File.ReadAllText(file)
                let newVals = Newtonsoft.Json.JsonConvert.DeserializeObject<Dictionary<string, Dictionary<string, string>>>(json)
                for x in newVals do
                    vals.[x.Key] <- x.Value
            with e ->
                printfn "Error: %s" e.Message
        
        | None ->
            printfn "Sorry, I couldn't understand that"            
        loop()
    loop()
    0 // return an integer exit code

