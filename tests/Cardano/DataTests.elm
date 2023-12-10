module Cardano.DataTests exposing (..)

import Bytes.Comparable as Bytes exposing (bytes)
import Cardano.Data as Data exposing (Data(..))
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Test as Cbor
import Expect
import Fuzz exposing (Fuzzer)
import Hex.Convert.Extra as Hex
import Test exposing (Test, describe, test)
import Tests exposing (expectBytes)


fuzzer : Fuzzer Data
fuzzer =
    let
        byte =
            Fuzz.intRange 0 255

        leaf =
            Fuzz.oneOf
                [ Fuzz.map Int <|
                    Fuzz.oneOf
                        [ Fuzz.int
                        , Fuzz.intRange -(2 ^ 51) -(2 ^ 50)
                        , Fuzz.intRange (2 ^ 50) (2 ^ 51)
                        ]
                , Fuzz.map (Bytes << bytes) <|
                    Fuzz.oneOf
                        [ Fuzz.list byte
                        , Fuzz.listOfLengthBetween 65 130 byte
                        ]
                ]

        whole depth =
            if depth <= 0 then
                leaf

            else
                let
                    nested =
                        whole (depth - 1)
                in
                Fuzz.oneOf
                    [ Fuzz.map List <|
                        Fuzz.listOfLengthBetween 0 depth nested
                    , Fuzz.map Map <|
                        Fuzz.listOfLengthBetween 0 depth <|
                            Fuzz.map2 Tuple.pair nested nested
                    , Fuzz.map2 Constr
                        (Fuzz.intRange 0 199)
                        (Fuzz.listOfLengthBetween 0 depth nested)
                    , leaf
                    ]
    in
    whole 3


suite : Test
suite =
    describe "Data"
        [ describe "toCbor"
            [ testEncode "D87980" <|
                Constr 0 []
            , testEncode "80" <|
                List []
            , testEncode "9F010203FF" <|
                List [ Int 1, Int 2, Int 3 ]
            , testEncode "A0" <|
                Map []
            , testEncode "A201020304" <|
                Map [ ( Int 1, Int 2 ), ( Int 3, Int 4 ) ]
            , testEncode "00" <|
                Int 0
            , testEncode "20" <|
                Int -1
            , testEncode "40" <|
                (Bytes <| bytes [])
            , testEncode "D87A9F21D87E9FD87C9F2143C2599BFF01FFD87C9F41B19F0044A06D8DCBFF40FFFF" <|
                Constr 1
                    [ Int -2
                    , Constr 5 [ Constr 3 [ Int -2, Bytes (bytes [ 0xC2, 0x59, 0x9B ]) ], Int 1 ]
                    , Constr 3 [ Bytes (bytes [ 0xB1 ]), List [ Int 0, Bytes (bytes [ 0xA0, 0x6D, 0x8D, 0xCB ]) ], Bytes (bytes []) ]
                    ]
            ]
        , describe "toCbor >> fromCbor"
            [ Cbor.roundtrip Data.toCbor Data.fromCbor fuzzer
            ]
        ]


testEncode : String -> Data -> Test
testEncode bytes data =
    test (Debug.toString data) <|
        \_ -> data |> Data.toCbor |> E.encode |> Bytes.fromBytes |> expectBytes bytes


testDecode : String -> Data -> Test
testDecode bytes data =
    test (Debug.toString data) <|
        \_ -> bytes |> Hex.fromString |> D.decode Data.fromCbor |> Expect.equal (Just data)
