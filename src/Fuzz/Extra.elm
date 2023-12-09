module Fuzz.Extra exposing (..)

import Fuzz exposing (Fuzzer)
import Natural as N exposing (Natural)


natural : Fuzzer Natural
natural =
    Fuzz.intAtLeast 0
        |> Fuzz.map N.fromSafeInt


strictPositiveNatural : Fuzzer Natural
strictPositiveNatural =
    Fuzz.intAtLeast 1
        |> Fuzz.map N.fromSafeInt
