module Tests exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Expect


expectBytes : String -> Bytes -> Expect.Expectation
expectBytes expected got =
    got |> Bytes.toString |> Expect.equal expected
