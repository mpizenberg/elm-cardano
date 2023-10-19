module ElmCardano.TransactionTests exposing (..)

import Bytes exposing (Bytes)
import ElmCardano.Transaction.Builder as Tx
import Expect
import Test exposing (Test, describe, test)
import Tests exposing (hex)


suite : Test
suite =
    describe "ElmCardano.Transaction"
        [ describe "Transaction.toCbor"
            [ test "basic encode" <|
                \_ ->
                    Tx.new
                        |> Tx.complete
                        |> expect
                            [ 0xAA
                            , 0xBB
                            ]
            ]
        ]


expect : List Int -> Bytes -> Expect.Expectation
expect fixture result =
    result |> hex |> Expect.equal (Just fixture)
