module Program
open System
open Statements
open Microsoft.FSharp.Text.Lexing
open System.Collections.Generic
open RollLib
open System.IO

[<EntryPoint>]
let main argv = 
    let interpreter = new Interpreter(printf "%s", Console.ReadLine, File.WriteAllText, File.ReadAllText)
    interpreter.exec()