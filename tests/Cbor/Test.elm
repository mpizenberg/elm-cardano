module Cbor.Test exposing (..)

import Bytes.Comparable as Bytes
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Expect
import Fuzz exposing (Fuzzer)
import Natural as N
import Test exposing (Test, fuzz)


roundtrip : (a -> E.Encoder) -> D.Decoder a -> Fuzzer a -> Test
roundtrip encode decode fuzzer =
    fuzz fuzzer "rountrip: Cbor.encode >> Cbor.decode" <|
        \a ->
            D.decode decode (E.encode (encode a))
                |> Expect.equal (Just a)


encodeNatural : Test
encodeNatural =
    Test.describe "Encode natural number"
        [ Test.test "Encode 2^64" <|
            \_ ->
                N.fromSafeString "0x10000000000000000"
                    |> EE.natural
                    |> E.encode
                    |> Bytes.fromBytes
                    |> Bytes.toHex
                    |> String.toUpper
                    |> Expect.equal "C249010000000000000000"
        ]
