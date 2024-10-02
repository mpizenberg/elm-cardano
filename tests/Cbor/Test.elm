module Cbor.Test exposing (..)

import Bytes.Comparable as Bytes
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Expect
import Fuzz exposing (Fuzzer)
import Integer as I
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
        , Test.test "Encode 2^64 - 1" <|
            \_ ->
                N.fromSafeString "0xFFFFFFFFFFFFFFFF"
                    |> EE.natural
                    |> E.encode
                    |> Bytes.fromBytes
                    |> Bytes.toHex
                    |> String.toUpper
                    |> Expect.equal "1BFFFFFFFFFFFFFFFF"
        , Test.test "Encode maxSafeInt + 1" <|
            \_ ->
                N.fromSafeInt N.maxSafeInt
                    |> N.add N.one
                    |> EE.natural
                    |> E.encode
                    |> Bytes.fromBytes
                    |> Bytes.toHex
                    |> String.toUpper
                    |> Expect.equal "1B0020000000000000"
        ]


encodeNegativeIntegers : Test
encodeNegativeIntegers =
    Test.describe "Encode negative numbers"
        [ Test.test "Encode -(2^64 + 1)" <|
            \_ ->
                I.fromSafeString "0x10000000000000001"
                    |> I.negate
                    |> EE.integer
                    |> E.encode
                    |> Bytes.fromBytes
                    |> Bytes.toHex
                    |> String.toUpper
                    |> Expect.equal "C349010000000000000000"
        , Test.test "Encode -(2^64)" <|
            \_ ->
                I.fromSafeString "0x10000000000000000"
                    |> I.negate
                    |> EE.integer
                    |> E.encode
                    |> Bytes.fromBytes
                    |> Bytes.toHex
                    |> String.toUpper
                    |> Expect.equal "3BFFFFFFFFFFFFFFFF"
        , Test.test "Encode -(maxSafeInt + 1)" <|
            \_ ->
                I.fromSafeInt I.maxSafeInt
                    |> I.add I.one
                    |> I.negate
                    |> EE.integer
                    |> E.encode
                    |> Bytes.fromBytes
                    |> Bytes.toHex
                    |> String.toUpper
                    |> Expect.equal "3B001FFFFFFFFFFFFF"
        ]
