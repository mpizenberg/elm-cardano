module Bytes.Comparable exposing
    ( Bytes
    , fromBytes, fromString, fromStringUnchecked
    , toBytes, toString
    )

{-| Comparable Bytes

@docs Bytes
@docs fromBytes, fromString, fromStringUnchecked
@docs toBytes, toString

-}

import Bytes
import Bytes.Encode as E
import Hex.Convert as Hex


type Bytes
    = Bytes String


fromString : String -> Maybe Bytes
fromString str =
    str |> Hex.toBytes |> Maybe.map (always <| Bytes str)


fromStringUnchecked : String -> Bytes
fromStringUnchecked =
    Bytes


fromBytes : Bytes.Bytes -> Bytes
fromBytes bytes =
    Bytes (Hex.toString bytes)


toString : Bytes -> String
toString (Bytes str) =
    str


toBytes : Bytes -> Bytes.Bytes
toBytes (Bytes str) =
    str |> Hex.toBytes |> Maybe.withDefault absurd


absurd : Bytes.Bytes
absurd =
    E.encode (E.sequence [])
