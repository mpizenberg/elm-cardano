module Tests exposing (..)

import Bytes exposing (Bytes)
import Expect
import Hex.Convert


expectBytes : String -> Bytes -> Expect.Expectation
expectBytes expected got =
    got |> Hex.Convert.toString |> Expect.equal expected
