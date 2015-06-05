module Roll.Tests

open Xunit
open Rolls

[<Fact>]
let ``Quit should return none``() = 
    Assert.Equal(None, Parser.Parse "quit")
    Assert.Equal(None, Parser.Parse "q")
    Assert.NotEqual(None, Parser.Parse "x")

[<Fact>]
let ``Roller should return rolls``() =
    Assert.Equal(6, Roller.Resolve <| Roll(6, 1, 0))