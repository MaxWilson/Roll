module Program
open System

// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let prompt msg processor = 
    printf "%s: " msg
    try
        processor ((Console.ReadLine()).Trim())
    with
        _ -> None
    
[<EntryPoint>]
let main argv = 
    printfn "%A" (prompt "What's your name?" (sprintf "Hello, %s" >> Some))
    0 // return an integer exit code

