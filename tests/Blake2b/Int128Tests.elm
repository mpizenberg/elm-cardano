module Blake2b.Int128Tests exposing (suite)

import Blake2b.Int128 as Int128 exposing (Int128(..))
import Expect
import Test exposing (Test, describe, test)
import UInt64


zero : Int128
zero =
    Int128 UInt64.zero UInt64.zero


one : Int128
one =
    Int128 UInt64.zero UInt64.one


suite : Test
suite =
    let
        u64 =
            UInt64.fromInt

        toHexEquals int128 correctHex =
            \_ -> Expect.equal (Int128.toHex int128 |> String.toLower) correctHex

        tests =
            [ test "Int128 0x6A09E667F3BCC908 0xBB67AE8584CAA73B"
            , test "Int128 0x3C6EF372FE94F82B 0xA54FF53A5F1D36F1"
            , test "Int128 0x510E527FADE682D1 0x9B05688C2B3E6C1F"
            , test "Int128 0x1F83D9ABFB41BD6B 0x5BE0CD19137E2179"
            , test "Int128 0x0000000000000000 0xFFFFFFFFFFFFFFFF"
            ]
    in
    describe "Int128"
        [ describe "toHex" <|
            List.map2
                (<|)
                tests
                [ toHexEquals sampleInt128_0 "6a09e667f3bcc908bb67ae8584caa73b"
                , toHexEquals sampleInt128_1 "3c6ef372fe94f82ba54ff53a5f1d36f1"
                , toHexEquals sampleInt128_2 "510e527fade682d19b05688c2b3e6c1f"
                , toHexEquals sampleInt128_3 "1f83d9abfb41bd6b5be0cd19137e2179"
                , toHexEquals sampleInt128_4 "0000000000000000ffffffffffffffff"
                ]
        , describe "multiply" <|
            [ test "0xFFFFFFFFFFFFFFFF x 0xFFFFFFFFFFFFFFFF" <|
                \_ ->
                    Expect.equal
                        (Int128.mul (UInt64.fromInt24s 0xFFFF 0x00FFFFFF 0x00FFFFFF) (UInt64.fromInt24s 0xFFFF 0x00FFFFFF 0x00FFFFFF))
                        (Int128 (UInt64.fromInt24s 0xFFFF 0x00FFFFFF 0x00FFFFFE) (UInt64.fromInt24s 0x00 0x00 0x01))
            ]
        ]


sampleInt128_0 : Int128
sampleInt128_0 =
    Int128
        (UInt64.fromInt32s 0x6A09E667 0xF3BCC908)
        (UInt64.fromInt32s 0xBB67AE85 0x84CAA73B)


sampleInt128_1 : Int128
sampleInt128_1 =
    Int128
        (UInt64.fromInt32s 0x3C6EF372 0xFE94F82B)
        (UInt64.fromInt32s 0xA54FF53A 0x5F1D36F1)


sampleInt128_2 : Int128
sampleInt128_2 =
    Int128
        (UInt64.fromInt32s 0x510E527F 0xADE682D1)
        (UInt64.fromInt32s 0x9B05688C 0x2B3E6C1F)


sampleInt128_3 : Int128
sampleInt128_3 =
    Int128
        (UInt64.fromInt32s 0x1F83D9AB 0xFB41BD6B)
        (UInt64.fromInt32s 0x5BE0CD19 0x137E2179)


sampleInt128_4 : Int128
sampleInt128_4 =
    Int128
        (UInt64.fromInt32s 0x00 0x00)
        (UInt64.fromInt32s 0xFFFFFFFF 0xFFFFFFFF)
