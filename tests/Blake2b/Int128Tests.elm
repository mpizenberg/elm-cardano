module Blake2b.Int128Tests exposing (suite)

import Blake2b.Int128 as Int128 exposing (Int128(..))
import Expect
import Test exposing (Test, describe, test)
import UInt64 exposing (UInt64)


suite : Test
suite =
    let
        toHexEquals int128 correctHex =
            \_ -> Expect.equal (Int128.toHex int128 |> String.toLower) correctHex

        tests =
            [ test "Int128 0x6A09E667F3BCC908 0xBB67AE8584CAA73B"
            , test "Int128 0x3C6EF372FE94F82B 0xA54FF53A5F1D36F1"
            , test "Int128 0x510E527FADE682D1 0x9B05688C2B3E6C1F"
            , test "Int128 0x1F83D9ABFB41BD6B 0x5BE0CD19137E2179"
            , test "Int128 0x0000000000000000 0xFFFFFFFFFFFFFFFF"
            ]

        mulHelper a b correctAnswer =
            test ("0x" ++ UInt64.toHexString a ++ " x 0x" ++ UInt64.toHexString b) <|
                \_ ->
                    Expect.equal
                        (Int128.mul a b)
                        correctAnswer
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
            [ mulHelper
                (UInt64.fromInt24s 0xFFFF 0x00FFFFFF 0x00FFFFFF)
                (UInt64.fromInt24s 0xFFFF 0x00FFFFFF 0x00FFFFFF)
                (Int128 (UInt64.fromInt24s 0xFFFF 0x00FFFFFF 0x00FFFFFE) (UInt64.fromInt24s 0x00 0x00 0x01))
            , mulHelper
                sampleInt64_0
                sampleInt64_1
                (Int128 (UInt64.fromInt24s 0x4DA0 0x002D93AB 0x009E5F5B) (UInt64.fromInt24s 0xF939 0x0075BC13 0x00F68CD8))
            , mulHelper
                sampleInt64_2
                sampleInt64_3
                (Int128 (UInt64.fromInt24s 0x2706 0x00630A35 0x0026DB80) (UInt64.fromInt24s 0x17D3 0x00F0AB28 0x0075B27B))
            , mulHelper
                sampleInt64_4
                sampleInt64_5
                (Int128 (UInt64.fromInt24s 0x3115 0x0062551D 0x00D31EBD) (UInt64.fromInt24s 0x20F5 0x0022EC17 0x00B8034F))
            , mulHelper
                sampleInt64_6
                sampleInt64_7
                (Int128 (UInt64.fromInt24s 0x0B4F 0x008AFE44 0x00618205) (UInt64.fromInt24s 0xBF66 0x00C78A67 0x00275293))
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


sampleInt128_0 : Int128
sampleInt128_0 =
    Int128 sampleInt64_0 sampleInt64_1


sampleInt128_1 : Int128
sampleInt128_1 =
    Int128 sampleInt64_2 sampleInt64_3


sampleInt128_2 : Int128
sampleInt128_2 =
    Int128 sampleInt64_4 sampleInt64_5


sampleInt128_3 : Int128
sampleInt128_3 =
    Int128 sampleInt64_6 sampleInt64_7


sampleInt128_4 : Int128
sampleInt128_4 =
    Int128
        (UInt64.fromInt32s 0x00 0x00)
        (UInt64.fromInt32s 0xFFFFFFFF 0xFFFFFFFF)
