module ElmCardano.CoinSelectionTests exposing (..)

import Bytes.Comparable as Bytes
import ElmCardano.CoinSelection exposing (CoinSelectionError(..), largestFirst)
import ElmCardano.Utxo exposing (Output(..))
import ElmCardano.Value exposing (onlyLovelace)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "ElmCardano.CoinSelection"
        [ describe "largestFirst"
            [ test "basic scenario" <| basicScenarioTest
            , test "no utxos" <| noUtxosTest
            , test "insufficient funds" <| insufficientFundsTest
            , test "single utxo, single output, equal value" <| singleUtxoSingleOutputEqualValueTest
            ]
        ]


basicScenarioTest : a -> Expect.Expectation
basicScenarioTest _ =
    let
        availableUtxos =
            [ Legacy { address = Bytes.fromStringUnchecked "addr1", amount = onlyLovelace 50, datumHash = Nothing }
            , Legacy { address = Bytes.fromStringUnchecked "addr2", amount = onlyLovelace 30, datumHash = Nothing }
            , Legacy { address = Bytes.fromStringUnchecked "addr3", amount = onlyLovelace 20, datumHash = Nothing }
            ]

        requestedOutputs =
            [ Legacy { address = Bytes.fromStringUnchecked "dest1", amount = onlyLovelace 30, datumHash = Nothing } ]

        nmax =
            5

        changeAddress =
            Bytes.fromStringUnchecked "changeAddr"

        args =
            { availableUtxos = availableUtxos
            , selectedUtxos = []
            , requestedOutputs = requestedOutputs
            , changeAddress = changeAddress
            }

        result =
            largestFirst args nmax

        expectedResult =
            Ok
                { selectedUtxos = [ Legacy { address = Bytes.fromStringUnchecked "addr1", amount = onlyLovelace 50, datumHash = Nothing } ]
                , requestedOutputs = requestedOutputs
                , changeOutput = Just (Legacy { address = changeAddress, amount = onlyLovelace 20, datumHash = Nothing })
                }
    in
    Expect.equal expectedResult result


noUtxosTest : a -> Expect.Expectation
noUtxosTest _ =
    let
        args =
            { availableUtxos = []
            , selectedUtxos = []
            , requestedOutputs = [ Legacy { address = Bytes.fromStringUnchecked "dest1", amount = onlyLovelace 30, datumHash = Nothing } ]
            , changeAddress = Bytes.fromStringUnchecked "changeAddr"
            }

        result =
            largestFirst args 5
    in
    Expect.equal (Err UTxOBalanceInsufficient) result


insufficientFundsTest : a -> Expect.Expectation
insufficientFundsTest _ =
    let
        availableUtxos =
            [ Legacy { address = Bytes.fromStringUnchecked "addr1", amount = onlyLovelace 5, datumHash = Nothing }
            , Legacy { address = Bytes.fromStringUnchecked "addr2", amount = onlyLovelace 10, datumHash = Nothing }
            ]

        args =
            { availableUtxos = availableUtxos
            , selectedUtxos = []
            , requestedOutputs = [ Legacy { address = Bytes.fromStringUnchecked "dest1", amount = onlyLovelace 30, datumHash = Nothing } ]
            , changeAddress = Bytes.fromStringUnchecked "changeAddr"
            }

        result =
            largestFirst args 5
    in
    Expect.equal (Err UTxOBalanceInsufficient) result


singleUtxoSingleOutputEqualValueTest : a -> Expect.Expectation
singleUtxoSingleOutputEqualValueTest _ =
    let
        args =
            { availableUtxos = [ Legacy { address = Bytes.fromStringUnchecked "addr1", amount = onlyLovelace 10, datumHash = Nothing } ]
            , selectedUtxos = []
            , requestedOutputs = [ Legacy { address = Bytes.fromStringUnchecked "dest1", amount = onlyLovelace 10, datumHash = Nothing } ]
            , changeAddress = Bytes.fromStringUnchecked "changeAddr"
            }

        result =
            largestFirst args 5

        expectedResult =
            Ok
                { selectedUtxos = args.availableUtxos
                , requestedOutputs = args.requestedOutputs
                , changeOutput = Nothing
                }
    in
    Expect.equal expectedResult result
