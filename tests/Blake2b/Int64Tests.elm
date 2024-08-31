module Blake2b.Int64Tests exposing (suite)

import Blake2b.Int64 as Int64 exposing (Int64(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        roundtrip =
            Int64.toLeByteValues >> Int64.fromLeByteValues

        unchangedAfterRoundtrip x =
            \_ -> Expect.equal (roundtrip x) (Just x)

        toHexEquals int64 correctHex =
            \_ -> Expect.equal (Int64.toHex int64 |> String.toLower) correctHex

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
        [ describe "Roundtrip `Int64.toLeByteValues >> Int64.fromLeByteValues`" <|
            List.map2
                (<|)
                tests
                [ unchangedAfterRoundtrip sampleInt64_0
                , unchangedAfterRoundtrip sampleInt64_1
                , unchangedAfterRoundtrip sampleInt64_2
                , unchangedAfterRoundtrip sampleInt64_3
                , unchangedAfterRoundtrip sampleInt64_4
                , unchangedAfterRoundtrip sampleInt64_5
                , unchangedAfterRoundtrip sampleInt64_6
                , unchangedAfterRoundtrip sampleInt64_7
                , unchangedAfterRoundtrip sampleInt64_8
                , unchangedAfterRoundtrip sampleInt64_9
                , unchangedAfterRoundtrip sampleInt64_10
                ]
        , describe "toHex" <|
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
        ]


sampleInt64_0 : Int64
sampleInt64_0 =
    Int64 0x6A09E667 0xF3BCC908


sampleInt64_1 : Int64
sampleInt64_1 =
    Int64 0xBB67AE85 0x84CAA73B


sampleInt64_2 : Int64
sampleInt64_2 =
    Int64 0x3C6EF372 0xFE94F82B


sampleInt64_3 : Int64
sampleInt64_3 =
    Int64 0xA54FF53A 0x5F1D36F1


sampleInt64_4 : Int64
sampleInt64_4 =
    Int64 0x510E527F 0xADE682D1


sampleInt64_5 : Int64
sampleInt64_5 =
    Int64 0x9B05688C 0x2B3E6C1F


sampleInt64_6 : Int64
sampleInt64_6 =
    Int64 0x1F83D9AB 0xFB41BD6B


sampleInt64_7 : Int64
sampleInt64_7 =
    Int64 0x5BE0CD19 0x137E2179


sampleInt64_8 : Int64
sampleInt64_8 =
    Int64 0x00 0x00


sampleInt64_9 : Int64
sampleInt64_9 =
    Int64 0xFFFFFFFF 0xFFFFFFFF


sampleInt64_10 : Int64
sampleInt64_10 =
    Int64 0x00 0x01
