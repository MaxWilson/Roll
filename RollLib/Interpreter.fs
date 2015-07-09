namespace RollLib

open System
open Statements
open Microsoft.FSharp.Text.Lexing
open System.Collections.Generic

type Interpreter(print : string -> unit, readline : unit -> string, writeFile : string * string -> unit, readFile : string -> string) =

    let rec printTokens stream =
        match Lexer.tokenstream stream with
        | Parser.EOF -> ()
        | token -> print <| sprintf "(%A) \n" token
                   printTokens stream

    member this.prompt msg processor = 
        print <| sprintf "%s " msg
        let input : string = (readline()).Trim()
        try
            Some (processor input)
        with        
            _ -> 
                try
                    input |> LexBuffer<char>.FromString |> printTokens
                with _ -> print "Could not tokenize\n"
                None

    member this.Parse input =     
        let parsed = input |> LexBuffer<char>.FromString |> Parser.start Lexer.tokenstream 
        #if DEBUG
        print <| sprintf "%A\n" parsed
        #endif
        parsed

    member this.exec() = 
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
        let logAction (creature : Dictionary<string, string>) txt =
            let actionTxt = 
                if creature.ContainsKey("action_log") then
                    sprintf "%s\n    %s" (creature.["action_log"]) txt
                else
                    txt
            creature.["action_log"] <- actionTxt
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
                print <| sprintf "[%s] %s: %s\n" (next.Value.Value.["initval"]) next.Value.Key (next.Value.Value.["action"])
                defaultOwner := next.Value.Value
            else
                defaultOwner := new Dictionary<string, string>()
                print "Done with round\n"

        let rec loop() =
            let mutable doLoop = true
            match this.prompt ">" (this.Parse) with
            | Some(Statements.QuitCommand) -> doLoop <- false
            | Some(Statements.RollCommand(rolls)) ->
                Roller.Resolve rolls (print << sprintf "%s\n") (print << sprintf "%d crits\n")
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
                    let printvals (name : string option) (dict : Dictionary<string, string>) =
                        match name with
                        | Some(name) ->
                            print <| sprintf "%s:\n" name
                        | None ->
                            match vals |> Seq.tryFind (fun kv -> kv.Value = dict) with
                            | Some(kv) ->
                                print <| sprintf "%s:\n" kv.Key
                            | _ -> ()
                        for x in dict do
                            if property.IsNone || property.Value = x.Key then
                                print <| sprintf "  %s: %s\n" x.Key x.Value
                    if name = Some("all") then
                        for x in vals do
                            printvals (Some x.Key) x.Value
                    else
                        let x = getCreature name
                        printvals name x
                    print "\n" 
            | Some(Delay(name)) ->
                let creature = getCreature name
                let initval = (creature.TryGetValue("initval") |> snd |> System.Int32.TryParse |> snd)
                creature.["initval"] <- (initval - 100).ToString()
                setNext()
            | Some(ResolveAction(owner, txt)) ->
                for creature in vals do
                    if creature.Value.ContainsKey("action") && (not <| creature.Value.ContainsKey("initval")) then
                        let init = 
                            match creature.Value.TryGetValue("init") with
                            | true, v -> v |> System.Int32.TryParse |> snd
                            | false, _ -> 0
                        let initval = Roller.ResolveBase (Roll(1,20, init)) (fun x -> ())
                        print <| sprintf "%s init: %d\n" creature.Key initval
                        creature.Value.["initval"] <- initval.ToString() 
                let ownerObject = getCreature owner
                if ownerObject.ContainsKey("action") then
                    logAction ownerObject (ownerObject.["action"])
                    if txt.IsSome then
                        logAction ownerObject ("  " + txt.Value)
                    
                    ownerObject.Remove("action") |> ignore
                    ownerObject.Remove("initval") |> ignore
                setNext()
            | Some(Log(name, txt)) ->
                let creature = getCreature name
                logAction creature txt
            | Some(Delete(name, property)) ->
                if property.IsNone then
                    vals.Remove(name.Value) |> ignore
                else
                    let creature = getCreature name
                    creature.Remove(property.Value) |> ignore
            | Some(Save(file)) ->
                let json = Newtonsoft.Json.JsonConvert.SerializeObject(vals)
                let file = if file.Contains(".json") then file else file + ".json"
                try
                    writeFile(file, json)
                with e ->
                    print <| sprintf "Error: %s\n" e.Message
            | Some(Load(file)) ->
                let file = if file.Contains(".json") then file else file + ".json"
                try
                    let json = readFile(file)
                    let newVals = Newtonsoft.Json.JsonConvert.DeserializeObject<Dictionary<string, Dictionary<string, string>>>(json)
                    for x in newVals do
                        vals.[x.Key] <- x.Value
                with e ->
                    print <| sprintf "Error: %s\n" e.Message
        
            | None ->
                print "Sorry, I couldn't understand that\n"            
            if doLoop then
                loop()
        loop()
        0 // return an integer exit code

