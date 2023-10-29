module ElmCardano.CoinSelectionTests exposing (..)

import ElmCardano.CoinSelection exposing (CoinSelectionError(..), largestFirst)
import ElmCardano.Transaction exposing (Output(..), Value(..))
import Expect
import Test exposing (Test, describe, test)
import Tests exposing (fromString)


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
        availableUtxo =
            [ Legacy { address = fromString "addr1", amount = Coin 50, datumHash = Nothing }
            , Legacy { address = fromString "addr2", amount = Coin 30, datumHash = Nothing }
            , Legacy { address = fromString "addr3", amount = Coin 20, datumHash = Nothing }
            ]

        requestedOutputs =
            [ Legacy { address = fromString "dest1", amount = Coin 30, datumHash = Nothing } ]

        nmax =
            5

        changeAddress =
            fromString "changeAddr"

        args =
            { availableUtxo = availableUtxo
            , selectedUtxos = []
            , requestedOutputs = requestedOutputs
            , changeAddress = changeAddress
            }

        result =
            largestFirst args nmax

        expectedResult =
            Ok
                { selectedUtxos = [ Legacy { address = fromString "addr1", amount = Coin 50, datumHash = Nothing } ]
                , requestedOutputs = requestedOutputs
                , changeOutput = Just (Legacy { address = changeAddress, amount = Coin 20, datumHash = Nothing })
                }
    in
    Expect.equal expectedResult result


noUtxosTest : a -> Expect.Expectation
noUtxosTest _ =
    let
        args =
            { availableUtxo = []
            , selectedUtxos = []
            , requestedOutputs = [ Legacy { address = fromString "dest1", amount = Coin 30, datumHash = Nothing } ]
            , changeAddress = fromString "changeAddr"
            }

        result =
            largestFirst args 5
    in
    Expect.equal (Err UTxOBalanceInsufficient) result


insufficientFundsTest : a -> Expect.Expectation
insufficientFundsTest _ =
    let
        availableUtxo =
            [ Legacy { address = fromString "addr1", amount = Coin 5, datumHash = Nothing }
            , Legacy { address = fromString "addr2", amount = Coin 10, datumHash = Nothing }
            ]

        args =
            { availableUtxo = availableUtxo
            , selectedUtxos = []
            , requestedOutputs = [ Legacy { address = fromString "dest1", amount = Coin 30, datumHash = Nothing } ]
            , changeAddress = fromString "changeAddr"
            }

        result =
            largestFirst args 5
    in
    Expect.equal (Err UTxOBalanceInsufficient) result

singleUtxoSingleOutputEqualValueTest : a -> Expect.Expectation
singleUtxoSingleOutputEqualValueTest _ =
    let
        args = 
            { availableUtxo = [ Legacy { address = fromString "addr1", amount = Coin 10, datumHash = Nothing } ]
            , selectedUtxos = []
            , requestedOutputs = [ Legacy { address = fromString "dest1", amount = Coin 10, datumHash = Nothing } ]
            , changeAddress = fromString "changeAddr"
            }
        result = largestFirst args 5
        expectedResult =
            Ok
                { selectedUtxos = args.availableUtxo
                , requestedOutputs = args.requestedOutputs
                , changeOutput = Nothing
                }
    in
    Expect.equal expectedResult result