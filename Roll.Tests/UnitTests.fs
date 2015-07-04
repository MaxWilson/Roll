module Roll.Tests

open Xunit
open Statements
open Xunit.Abstractions
open Microsoft.FSharp.Text.Lexing
open RollLib
open System
open System.IO

let interpreter = new Interpreter(printf "%s", Console.ReadLine, File.WriteAllText, File.ReadAllText)

let parse input = 
    try
        let parsed = interpreter.Parse input
        match parsed with
        | RollCommand(roll) -> roll
        | _ -> failwith "Unexpected output"
    with _ ->
        let rec printTokens stream =
            match Lexer.tokenstream stream with
            | Parser.EOF -> ""
            | token -> 
                        (sprintf "(%A) " token) + (printTokens stream)
        failwith <| sprintf "Could not parse '%s'" (input |> LexBuffer<char>.FromString |> printTokens)


type Unit(output: ITestOutputHelper) =
    [<Fact>]
    let ``Quit should return none``() = 
        let parse = interpreter.Parse
        Assert.Equal(QuitCommand, parse "quit")
        Assert.Equal(QuitCommand, parse "q")
        Assert.NotEqual(QuitCommand, parse "2d4")
        
    [<Fact>]
    let ``Spot check SetValue commands``() =
        let eq(lhs : Command, rhs : Command) =
            if lhs <> rhs then
                output.WriteLine(sprintf "%A != %A" lhs rhs)
                Assert.Equal(lhs, rhs)
        eq(SetValue(None, "status", "still here"), interpreter.Parse @"set status ""still here""")
        eq(SetValue(Some "jack", "HP", "30"), 
            interpreter.Parse("set jack HP 30"))
        eq(Delete(Some "umberhulk_1", None), interpreter.Parse @"kill umberhulk_1")
        eq(Delete(Some "umberhulk1", None), interpreter.Parse @"kill umberhulk1")
        eq(Delete(Some "umberhulk1", Some "stunned"), interpreter.Parse @"delete umberhulk1 stunned")

    [<Fact>]
    let ``Spot check sums``() =
        let eq(lhs : RollPrimitive, rhs : RollPrimitive) =
            if lhs <> rhs then
                output.WriteLine(sprintf "%A != %A" lhs rhs)
                Assert.Equal(lhs, rhs)
        eq(Statements.Roll(1, 8, 6), Statements.MakeSum(Statements.Roll(1,8,0), Statements.Roll(0,0,6)))
        eq(Statements.Roll(1, 8, 6), Statements.MakeSum(Statements.Roll(1,8,3), Statements.Roll(0,0,3)))
        eq(Statements.Sum([Statements.Roll(4,10,5); Statements.Roll(1,6,5)]), 
            Statements.MakeSum(Statements.Roll(4,10,5), Statements.Roll(1,6,5)))
        
    [<Fact>]
    let ``Simple expressions should parse correctly``() =
        let parse input = 
            try
                match interpreter.Parse input with
                | RollCommand(Simple(roll)) -> roll
                | _ -> failwith "Unexpected output"
            with _ ->
                let rec printTokens stream =
                    match Lexer.tokenstream stream with
                    | Parser.EOF -> ""
                    | token -> 
                                (sprintf "(%A) " token) + (printTokens stream)
                failwith <| sprintf "Could not parse '%s'" (input |> LexBuffer<char>.FromString |> printTokens)

        let eq(lhs : RollPrimitive, rhs : RollPrimitive) =
            if lhs <> rhs then
                output.WriteLine(sprintf "%A != %A" lhs rhs)
                Assert.Equal(lhs, rhs)
        eq(Statements.Sum [Roll(4,10,0); Roll(1,6,0)], parse("4d10+d6"))
        eq(Statements.Sum [Roll(4,10,0); Roll(1,6,5)], parse("4d10+d6+5"))
        eq(Statements.Roll(3,6,4), parse("3d6+4"))
        eq(Statements.Roll(3,6,0), parse("3d6"))
        eq(Statements.Roll(1,8,0), parse("d8"))
        eq(Statements.Roll(3,6,0), parse("3d"))
        eq(Statements.Roll(1,8,2), parse("d8+2"))
        eq(Statements.Roll(3,6,2), parse("3d+2"))
        eq(Statements.Roll(3,6,-2), parse("3d-2"))
        eq(Statements.Min [Roll(3,6,0); Roll(3,6,0)], parse("min 3d6,3d6"))
        eq(Statements.Max [Roll(3,6,0); Roll(3,6,0); Roll(1,8,0)], parse("max 3d6,3d6,1d8"))
        eq(Statements.Min [Roll(1,20,0); Roll(1,20,0)], parse("d20disadv"))
        eq(Statements.Max [Roll(1,20,0); Roll(1,20,0)], parse("d20adv"))
        eq(Statements.Sum [Roll(4,10,0); Roll(1,6,5)], parse("4d10+5+1d6"))
        eq(Statements.Sum [Roll(4,10,0); Roll(1,6,5)], parse("4d10+5+d6"))
        eq(Statements.Sum [Roll(4,10,0); Roll(5,6,0)], parse("4d10+0+5d6"))
        eq(Statements.Sum [Roll(4,10,0); Roll(1,6,5)], parse("4d10+d6+5"))
    [<Fact>]
    let ``Complex expressions should parse correctly``() =
        let eq(lhs : RollComplex, rhs) =
            if lhs <> rhs then
                output.WriteLine(sprintf "%A != %A" lhs rhs)
                Assert.Equal(lhs, rhs)
        eq(Statements.Repeat(2, Simple(Roll(3,6,0))), parse("2.3d6"))
        eq(Statements.AtLeast(Simple(Statements.Roll(1,20,0)), 14, 20), parse("d20?14"))
        eq(Statements.Repeat(20, Statements.AtLeast(Simple(Statements.Roll(1,100,0)), 40, 95)), 
            parse("20.d100?40(95)"))

    [<Fact>]
    let ``Bad input will throw an exception``() =
        Assert.Throws<System.Exception>(fun () -> parse("x") |> ignore) |> ignore

    [<Fact>]
    let ``Whitespace should be ignored``() =
        ()
        
    [<Fact>]
    let ``Roller spot checks``() =
        let resolve roll = 
            Roller.ResolveComplex (Simple roll) ignore |> fst
        Assert.Equal(6, resolve <| Roll(6, 1, 0))
        let between lower upper n =
            let result = (lower <= n && n <= upper)
            if not result then
                output.WriteLine(sprintf "Failed: %d is not between %d and %d" n lower upper)
            result
        for _ in [1..100] do
            Assert.True(between 3 4 (resolve <| Roll(1, 2, 2)))
            Assert.True(between 3 5 (resolve <| Roll(1, 3, 2)))
