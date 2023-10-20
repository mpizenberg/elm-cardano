module ElmCardano.TransactionTests exposing (..)

import Bytes exposing (Bytes)
import Bytes.Encode as E
import ElmCardano.Core exposing (NetworkId(..))
import ElmCardano.Transaction.Builder as Tx
import Expect
import Hex.Convert
import Test exposing (Test, describe, test)
import Tests exposing (fromString)


suite : Test
suite =
    describe "ElmCardano.Transaction"
        [ describe "Transaction.toCbor"
            [ test "basic encode" <|
                \_ ->
                    let
                        transactionId =
                            fromString "9D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6"
                    in
                    Tx.new Mainnet
                        |> Tx.input { transactionId = transactionId, outputIndex = 1 }
                        |> Tx.input { transactionId = transactionId, outputIndex = 0 }
                        |> Tx.payToAddress (E.sequence [] |> E.encode) 1000000
                        |> Tx.complete
                        |> expect "84A900828258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6008258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6010182A300581D70589144CC521615315237F12698F063220EFA4BC2F315B6C6E718A6D5011A02FAF080028201D8185822D8799F581CDD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAEFF82581D60DD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1A7415FABE021A0002C81E0B5820F90CF11D0959B9AF8E6FCE107ACD7A196C21FA3A0D9F1470A8CDEC905DCC6D850D818258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6010E81581CDD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1082581D60DD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1A7119A62F111A00042C2D1281825820517B059959FC8EE584689F71CF1D9BB94FC36802AEC0FAA7FD96182C0AB090C400A2049FD87980FF0581840000D879808219C11B1A011750D5F5F6"
            ]
        ]


expect : String -> Bytes -> Expect.Expectation
expect fixture result =
    result |> Hex.Convert.toString |> Expect.equal fixture
