module Roll.Tests

open Xunit
open Rolls
open Xunit.Abstractions
open DiceParser 

type Unit(output: ITestOutputHelper) =
    [<Fact>]
    let ``Quit should return none``() = 
        Assert.Equal(QuitCommand, DiceParser.Parse "quit")
        Assert.Equal(QuitCommand, DiceParser.Parse "q")
        Assert.NotEqual(QuitCommand, DiceParser.Parse "2d4")

    [<Fact>]
    let ``Simple expressions should parse correctly``() =
        let eq(lhs : RollSpec, rhs : RollSpec) =
            if lhs <> rhs then
                output.WriteLine(sprintf "%A != %A" lhs rhs)
                Assert.Equal(lhs, rhs)
        eq(Rolls.Roll(3,6,4), DiceParser.ParseDice("3d6+4"))
        eq(Rolls.Roll(3,6,0), DiceParser.ParseDice("3d6"))
        eq(Rolls.Roll(1,8,0), DiceParser.ParseDice("d8"))
        eq(Rolls.Roll(3,6,0), DiceParser.ParseDice("3d"))
        eq(Rolls.Roll(1,8,2), DiceParser.ParseDice("d8+2"))
        eq(Rolls.Roll(3,6,2), DiceParser.ParseDice("3d+2"))
        eq(Rolls.Roll(3,6,-2), DiceParser.ParseDice("3d-2"))
        eq(Rolls.Repeat(2, Roll(3,6,0)), DiceParser.ParseDice("2.3d6"))
        eq(Rolls.Min [Roll(3,6,0); Roll(3,6,0)], DiceParser.ParseDice("min 3d6,3d6"))
        eq(Rolls.Max [Roll(3,6,0); Roll(3,6,0); Roll(1,8,0)], DiceParser.ParseDice("max 3d6,3d6,1d8"))
        eq(Rolls.Min [Roll(1,20,0); Roll(1,20,0)], DiceParser.ParseDice("d20disad"))
        eq(Rolls.Max [Roll(1,20,0); Roll(1,20,0)], DiceParser.ParseDice("d20adv"))
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
