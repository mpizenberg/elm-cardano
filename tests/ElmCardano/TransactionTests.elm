module ElmCardano.TransactionTests exposing (..)

import Bytes exposing (Bytes)
import Bytes.Encode as E
import ElmCardano.Core exposing (NetworkId(..))
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
                    Tx.new Mainnet
                        |> Tx.input
                            { transactionId =
                                List.repeat 32 0
                                    |> List.map E.unsignedInt8
                                    |> E.sequence
                                    |> E.encode
                            , outputIndex = 0
                            }
                        |> Tx.payToAddress (E.sequence [] |> E.encode) 1000000
                        |> Tx.complete
                        |> expect
                            [ 0x9F
                            , 0xBB
                            ]
            ]
        ]


expect : List Int -> Bytes -> Expect.Expectation
expect fixture result =
    result |> hex |> Expect.equal (Just fixture)
