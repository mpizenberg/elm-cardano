module Blake2b.Int64Tests exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import UInt64 exposing (UInt64)


suite : Test
suite =
    let
        zero =
            UInt64.zero

        one =
            UInt64.one

        u64 =
            UInt64.fromInt

        toHexEquals int64 correctHex =
            \_ -> Expect.equal (UInt64.toHexString int64 |> String.toLower) correctHex

        tests =
            [ test "Int64 0x6A09E667 0xF3BCC908"
            , test "Int64 0xBB67AE85 0x84CAA73B"
            , test "Int64 0x3C6EF372 0xFE94F82B"
            , test "Int64 0xA54FF53A 0x5F1D36F1"
            , test "Int64 0x510E527F 0xADE682D1"
            , test "Int64 0x9B05688C 0x2B3E6C1F"
            , test "Int64 0x1F83D9AB 0xFB41BD6B"
            , test "Int64 0x5BE0CD19 0x137E2179"
            , test "Int64 0x00000000 0x00000000"
            , test "Int64 0xFFFFFFFF 0xFFFFFFFF"
            , test "Int64 0x00000000 0x00000001"
            ]
    in
    describe "Int64"
        [ describe "toHex" <|
            List.map2
                (<|)
                tests
                [ toHexEquals sampleInt64_0 "6a09e667f3bcc908"
                , toHexEquals sampleInt64_1 "bb67ae8584caa73b"
                , toHexEquals sampleInt64_2 "3c6ef372fe94f82b"
                , toHexEquals sampleInt64_3 "a54ff53a5f1d36f1"
                , toHexEquals sampleInt64_4 "510e527fade682d1"
                , toHexEquals sampleInt64_5 "9b05688c2b3e6c1f"
                , toHexEquals sampleInt64_6 "1f83d9abfb41bd6b"
                , toHexEquals sampleInt64_7 "5be0cd19137e2179"
                , toHexEquals sampleInt64_8 "0000000000000000"
                , toHexEquals sampleInt64_9 "ffffffffffffffff"
                , toHexEquals sampleInt64_10 "0000000000000001"
                ]
        , describe "multiply" <|
            [ test "0x00 x 0x01" <|
                \_ ->
                    Expect.equal
                        (UInt64.mul zero one)
                        zero
            , test "0x01 x 0x01" <|
                \_ ->
                    Expect.equal
                        (UInt64.mul one one)
                        one
            , test "0x01 x 0xFFFFFFFF" <|
                \_ ->
                    Expect.equal
                        (UInt64.mul one (u64 0xFFFFFFFF))
                        (u64 0xFFFFFFFF)
            , test "0x02 x 0xFFFFFFFF" <|
                \_ ->
                    Expect.equal
                        (UInt64.mul UInt64.two (u64 0xFFFFFFFF))
                        (UInt64.fromInt32s 1 0xFFFFFFFE)
            , test "0xFFFFFFFF x 0xFFFFFFFF" <|
                \_ ->
                    Expect.equal
                        (UInt64.mul (u64 0xFFFFFFFF) (u64 0xFFFFFFFF))
                        (UInt64.fromInt32s 0xFFFFFFFE 0x01)
            , test "0x6A09E667 x 0xF3BCC908" <|
                \_ ->
                    Expect.equal
                        (UInt64.mul (u64 0x6A09E667) (u64 0xF3BCC908))
                        (UInt64.fromInt32s 0x64F5983E 0x0EDA1238)
            ]
        ]


sampleInt64_0 : UInt64
sampleInt64_0 =
    UInt64.fromInt32s 0x6A09E667 0xF3BCC908


sampleInt64_1 : UInt64
sampleInt64_1 =
    UInt64.fromInt32s 0xBB67AE85 0x84CAA73B


sampleInt64_2 : UInt64
sampleInt64_2 =
    UInt64.fromInt32s 0x3C6EF372 0xFE94F82B


sampleInt64_3 : UInt64
sampleInt64_3 =
    UInt64.fromInt32s 0xA54FF53A 0x5F1D36F1


sampleInt64_4 : UInt64
sampleInt64_4 =
    UInt64.fromInt32s 0x510E527F 0xADE682D1


sampleInt64_5 : UInt64
sampleInt64_5 =
    UInt64.fromInt32s 0x9B05688C 0x2B3E6C1F


sampleInt64_6 : UInt64
sampleInt64_6 =
    UInt64.fromInt32s 0x1F83D9AB 0xFB41BD6B


sampleInt64_7 : UInt64
sampleInt64_7 =
    UInt64.fromInt32s 0x5BE0CD19 0x137E2179


sampleInt64_8 : UInt64
sampleInt64_8 =
    UInt64.fromInt32s 0x00 0x00


sampleInt64_9 : UInt64
sampleInt64_9 =
    UInt64.fromInt32s 0xFFFFFFFF 0xFFFFFFFF


sampleInt64_10 : UInt64
sampleInt64_10 =
    UInt64.fromInt32s 0x00 0x01
