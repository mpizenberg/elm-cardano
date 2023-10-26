module ComparableBytes exposing
    ( ComparableBytes
    , fromBytes, fromStringUnchecked
    , asHex, asBytes
    )

{-| Comparable Bytes

@docs ComparableBytes
@docs fromBytes, fromStringUnchecked
@docs asHex, asBytes

-}

import Bytes exposing (Bytes)
import Bytes.Encode as E
import Hex.Convert as Hex


type ComparableBytes
    = ComparableBytes String


fromStringUnchecked : String -> ComparableBytes
fromStringUnchecked str =
    ComparableBytes str


fromBytes : Bytes -> ComparableBytes
fromBytes bytes =
    ComparableBytes (Hex.toString bytes)


asHex : ComparableBytes -> String
asHex (ComparableBytes hexStr) =
    hexStr


asBytes : ComparableBytes -> Bytes
asBytes (ComparableBytes hexStr) =
    unhex hexStr


unhex : String -> Bytes
unhex hexStr =
    Maybe.withDefault absurd (Hex.toBytes hexStr)


absurd : Bytes
absurd =
    E.encode (E.sequence [])
