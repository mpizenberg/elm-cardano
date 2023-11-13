module Tests exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Expect


expectBytes : String -> Bytes a -> Expect.Expectation
expectBytes expected got =
    got |> Bytes.toString |> Expect.equal (String.toLower expected)
