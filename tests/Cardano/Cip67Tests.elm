module Cardano.Cip67Tests exposing (suite)

import Cardano.Cip67 as Cip67
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Cip67" <|
        -- https://github.com/cardano-foundation/CIPs/blob/d6c5ad3a77b4684bc19f3cafb75b4886a67c9a31/CIP-0067/README.md?plain=1#L115
        [ describe "Test Vectors" <|
            [ test "cip-67 label 0 == 00000000" <| \_ -> equalLabel 0 "00000000"
            , test "cip-67 label 1 == 00001070" <| \_ -> equalLabel 1 "00001070"
            , test "cip-67 label 23 == 00017650" <| \_ -> equalLabel 23 "00017650"
            , test "cip-67 label 99 == 000632e0" <| \_ -> equalLabel 99 "000632e0"
            , test "cip-67 label 533 == 00215410" <| \_ -> equalLabel 533 "00215410"
            , test "cip-67 label 2000 == 007d0550" <| \_ -> equalLabel 2000 "007d0550"
            , test "cip-67 label 4567 == 011d7690" <| \_ -> equalLabel 4567 "011d7690"
            , test "cip-67 label 11111 == 02b670b0" <| \_ -> equalLabel 11111 "02b670b0"
            , test "cip-67 label 49328 == 0c0b0f40" <| \_ -> equalLabel 49328 "0c0b0f40"
            , test "cip-67 label 65535 == 0ffff240" <| \_ -> equalLabel 65535 "0ffff240"
            ]
        , fuzz (Fuzz.intRange 0 65535) "Fuzz round trips" <|
            \n ->
                Expect.equal (Just n) (Cip67.labelFromHex <| Cip67.labelToHex n)
        ]


equalLabel : Int -> String -> Expect.Expectation
equalLabel n digest =
    Expect.equal digest (Cip67.labelToHex n)
