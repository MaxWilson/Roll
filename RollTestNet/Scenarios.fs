module Scenarios

open Xunit

[<Theory>]
[<InlineData("2d4", "5")>]
[<InlineData("2d4+4", "9")>]
[<InlineData("2*2d4", "10")>]
[<InlineData("2.2d4", "10")>]
[<InlineData("2x2d4", "10")>]
[<InlineData("(2d4)", "5")>]
let CheckAverages input expected =
  let spec = Dice.parse input
  let actual = Dice.average spec
  Assert.Equal(expected, actual)