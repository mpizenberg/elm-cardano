module ElmCardano.TransactionTests exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import ElmCardano.Core exposing (NetworkId(..))
import ElmCardano.Data exposing (Data(..))
import ElmCardano.Hash as Hash
import ElmCardano.Redeemer exposing (RedeemerTag(..))
import ElmCardano.Transaction.Builder as Tx
import Test exposing (Test, describe, test)
import Tests exposing (expectBytes)


suite : Test
suite =
    describe "ElmCardano.Transaction"
        [ describe "Transaction.toCbor"
            [ test "basic encode" <|
                \_ ->
                    let
                        transactionId =
                            Hash.blake2b_256 "9D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6"
                    in
                    Tx.new
                        |> Tx.input { transactionId = transactionId, outputIndex = 1 }
                        |> Tx.input { transactionId = transactionId, outputIndex = 0 }
                        |> Tx.inputData (Constr 0 [])
                        |> Tx.redeemer
                            { tag = Spend
                            , index = 0
                            , data = Constr 0 []
                            , exUnits = { mem = 49435, steps = 18305237 }
                            }
                        |> Tx.payToContract
                            (Bytes.fromStringUnchecked "70589144cc521615315237f12698f063220efa4bc2f315b6c6e718a6d5")
                            50000000
                            (Constr 0 [ Bytes <| Bytes.fromStringUnchecked "dd4edd90a2299da2525053c5e18e7c72625f7cf926f5731139d93bae" ])
                        |> Tx.payToAddress (Bytes.fromStringUnchecked "60dd4edd90a2299da2525053c5e18e7c72625f7cf926f5731139d93bae") 1947597502
                        |> Tx.fee 182302
                        |> Tx.scriptDataHash (Bytes.fromStringUnchecked "f90cf11d0959b9af8e6fce107acd7a196c21fa3a0d9f1470a8cdec905dcc6d85")
                        |> Tx.collateral { transactionId = transactionId, outputIndex = 1 }
                        |> Tx.requiredSigner (Hash.blake2b_224 "dd4edd90a2299da2525053c5e18e7c72625f7cf926f5731139d93bae")
                        |> Tx.collateralReturn (Bytes.fromStringUnchecked "60dd4edd90a2299da2525053c5e18e7c72625f7cf926f5731139d93bae") 1897506351
                        |> Tx.totalCollateral 273453
                        |> Tx.referenceInput { transactionId = Hash.blake2b_256 "517b059959fc8ee584689f71cf1d9bb94fc36802aec0faa7fd96182c0ab090c4", outputIndex = 0 }
                        |> Tx.complete
                        |> expectBytes "84A900828258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6008258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6010182A300581D70589144CC521615315237F12698F063220EFA4BC2F315B6C6E718A6D5011A02FAF080028201D8185822D8799F581CDD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAEFF82581D60DD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1A7415FABE021A0002C81E0B5820F90CF11D0959B9AF8E6FCE107ACD7A196C21FA3A0D9F1470A8CDEC905DCC6D850D818258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6010E81581CDD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1082581D60DD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1A7119A62F111A00042C2D1281825820517B059959FC8EE584689F71CF1D9BB94FC36802AEC0FAA7FD96182C0AB090C400A2049FD87980FF0581840000D879808219C11B1A011750D5F5F6"
            ]
        ]
