module Crc8Tests exposing (suite)

import Bytes
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Crc8 as Crc8
import Bytes.Encode as BE
import Expect
import Test exposing (Test, describe, test)
import Tests exposing (expectBytes)


suite : Test
suite =
    -- https://github.com/cardano-foundation/CIPs/blob/d6c5ad3a77b4684bc19f3cafb75b4886a67c9a31/CIP-0067/README.md?plain=1#L115
    describe "Crc8" <|
        [ test "crc8 0 == 0x00" <| \_ -> equalDigest 0 "00"
        , test "crc8 1 == 0x07" <| \_ -> equalDigest 1 "07"
        , test "crc8 23 == 0x65" <| \_ -> equalDigest 23 "65"
        , test "crc8 99 == 0x2e" <| \_ -> equalDigest 99 "2e"
        , test "crc8 533 == 0x41" <| \_ -> equalDigest 533 "41"
        , test "crc8 2000 == 0x55" <| \_ -> equalDigest 2000 "55"
        , test "crc8 4567 == 0x69" <| \_ -> equalDigest 4567 "69"
        , test "crc8 11111 == 0x0b" <| \_ -> equalDigest 11111 "0b"
        , test "crc8 49328 == 0xf4" <| \_ -> equalDigest 49328 "f4"
        , test "crc8 65535 == 0x24" <| \_ -> equalDigest 65535 "24"
        ]


equalDigest : Int -> String -> Expect.Expectation
equalDigest n digest =
    expectBytes digest (Crc8.digest <| u16Bytes n)


u16Bytes : Int -> Bytes a
u16Bytes n =
    BE.encode (BE.unsignedInt16 Bytes.BE n)
        |> Bytes.fromBytes
