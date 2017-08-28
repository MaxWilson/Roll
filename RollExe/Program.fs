// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open Dice

[<EntryPoint>]
let main argv =
  let prompt() =
    printf ">"
    Console.ReadLine().Trim()
  let rec loop() =
    let v = prompt()
    if v = "quit" || v = "q" then
      0
    else
      printfn "%s" (v |> parse |> eval)
      loop()
  loop()