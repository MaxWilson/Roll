namespace Rolls

type RollSpec = Roll of dice : int * size : int * plus : int
                | Max of rolls: RollSpec[]
                | Min of rolls: RollSpec[]
                | Sum of rolls: RollSpec[]

module Roller =
    let Roll spec = 0

