module Roll.Tests

open Xunit
open Statements
open Xunit.Abstractions
open DiceParser 

type Unit(output: ITestOutputHelper) =
    [<Fact>]
    let ``Quit should return none``() = 
        Assert.Equal(QuitCommand, DiceParser.Parse "quit")
        Assert.Equal(QuitCommand, DiceParser.Parse "q")
        Assert.NotEqual(QuitCommand, DiceParser.Parse "2d4")

    [<Fact>]
    let ``Spot check sums``() =
        let eq(lhs : RollSpec, rhs : RollSpec) =
            if lhs <> rhs then
                output.WriteLine(sprintf "%A != %A" lhs rhs)
                Assert.Equal(lhs, rhs)
        eq(Statements.Roll(1, 8, 6), Statements.MakeSum(Statements.Roll(1,8,0), Statements.Roll(0,0,6)))
        eq(Statements.Roll(1, 8, 6), Statements.MakeSum(Statements.Roll(1,8,3), Statements.Roll(0,0,3)))
        eq(Statements.Sum([Statements.Roll(4,10,5); Statements.Roll(1,6,5)]), 
            Statements.MakeSum(Statements.Roll(4,10,5), Statements.Roll(1,6,5)))
        
    [<Fact>]
    let ``Simple expressions should parse correctly``() =
        let eq(lhs : RollSpec, rhs : RollSpec) =
            if lhs <> rhs then
                output.WriteLine(sprintf "%A != %A" lhs rhs)
                Assert.Equal(lhs, rhs)
        eq(Statements.Sum [Roll(4,10,0); Roll(1,6,0)], DiceParser.ParseDice("4d10+d6"))
        eq(Statements.Sum [Roll(4,10,0); Roll(1,6,5)], DiceParser.ParseDice("4d10+d6+5"))
        eq(Statements.Roll(3,6,4), DiceParser.ParseDice("3d6+4"))
        eq(Statements.Roll(3,6,0), DiceParser.ParseDice("3d6"))
        eq(Statements.Roll(1,8,0), DiceParser.ParseDice("d8"))
        eq(Statements.Roll(3,6,0), DiceParser.ParseDice("3d"))
        eq(Statements.Roll(1,8,2), DiceParser.ParseDice("d8+2"))
        eq(Statements.Roll(3,6,2), DiceParser.ParseDice("3d+2"))
        eq(Statements.Roll(3,6,-2), DiceParser.ParseDice("3d-2"))
        eq(Statements.Repeat(2, Roll(3,6,0)), DiceParser.ParseDice("2.3d6"))
        eq(Statements.Min [Roll(3,6,0); Roll(3,6,0)], DiceParser.ParseDice("min 3d6,3d6"))
        eq(Statements.Max [Roll(3,6,0); Roll(3,6,0); Roll(1,8,0)], DiceParser.ParseDice("max 3d6,3d6,1d8"))
        eq(Statements.Min [Roll(1,20,0); Roll(1,20,0)], DiceParser.ParseDice("d20disadv"))
        eq(Statements.Max [Roll(1,20,0); Roll(1,20,0)], DiceParser.ParseDice("d20adv"))
        eq(Statements.Sum [Roll(4,10,0); Roll(1,6,5)], DiceParser.ParseDice("4d10+5+1d6"))
        eq(Statements.Sum [Roll(4,10,0); Roll(1,6,5)], DiceParser.ParseDice("4d10+5+d6"))
        eq(Statements.Sum [Roll(4,10,0); Roll(5,6,0)], DiceParser.ParseDice("4d10+0+5d6"))
        eq(Statements.Sum [Roll(4,10,0); Roll(1,6,5)], DiceParser.ParseDice("4d10+d6+5"))
        eq(Statements.AtLeast(Statements.Roll(1,20,0), 14), DiceParser.ParseDice("d20?14"))

    [<Fact>]
    let ``Bad input will throw an exception``() =
        Assert.Throws<System.Exception>(fun () -> DiceParser.Parse("x") |> ignore)

    [<Fact>]
    let ``Whitespace should be ignored``() =
        ()

    [<Fact>]
    let ``Roller spot checks``() =
        Assert.Equal(6, Roller.Resolve <| Roll(6, 1, 0))
        let between lower upper n =
            let result = (lower <= n && n <= upper)
            if not result then
                output.WriteLine(sprintf "Failed: %d is not between %d and %d" n lower upper)
            result
        for _ in [1..100] do
            Assert.True(between 3 4 (Roller.Resolve <| Roll(1, 2, 2)))
            Assert.True(between 3 5 (Roller.Resolve <| Roll(1, 3, 2)))
