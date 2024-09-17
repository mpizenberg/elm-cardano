module Fuzz.Extra exposing (bytes, bytesOfSize, natural, strictPositiveNatural)

import Bytes.Comparable as Bytes exposing (Bytes)
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


bytesOfSize : Int -> Fuzzer (Bytes a)
bytesOfSize size =
    Fuzz.listOfLength size (Fuzz.intRange 0 255)
        |> Fuzz.map Bytes.bytes


bytes : Fuzzer (Bytes a)
bytes =
    Fuzz.listOfLengthBetween 0 128 (Fuzz.intRange 0 255)
        |> Fuzz.map Bytes.bytes
