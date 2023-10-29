module ElmCardano.CoinSelectionTests exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import ElmCardano.CoinSelection exposing (Error(..), largestFirst)
import ElmCardano.Utxo exposing (Output(..), fromLovelace)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "CoinSelection"
        [ describe "largestFirst"
            [ test "basic scenario" <| basicScenarioTest
            , test "no utxos" <| noOutputsTest
            , test "insufficient funds" <| insufficientFundsTest
            , test "single utxo, single output, equal value" <| singleUtxoSingleOutputEqualValueTest
            ]
        ]


basicScenarioTest : a -> Expect.Expectation
basicScenarioTest _ =
    let
        context =
            { availableOutputs =
                [ fromLovelace (address "1") 50
                , fromLovelace (address "2") 30
                , fromLovelace (address "3") 20
                ]
            , alreadySelectedOutputs = []
            , targetAmount = 30
            , changeAddress =
                address "change"
            }

        nmax =
            5

        expectedResult =
            Ok
                { selectedOutputs = [ fromLovelace (address "1") 50 ]
                , changeOutput = Just <| fromLovelace context.changeAddress 20
                }
    in
    largestFirst context nmax
        |> Expect.equal expectedResult


noOutputsTest : a -> Expect.Expectation
noOutputsTest _ =
    let
        context =
            { availableOutputs = []
            , alreadySelectedOutputs = []
            , targetAmount = 30
            , changeAddress = address "change"
            }

        nmax =
            5
    in
    largestFirst context nmax
        |> Expect.equal (Err UTxOBalanceInsufficient)


insufficientFundsTest : a -> Expect.Expectation
insufficientFundsTest _ =
    let
        availableOutputs =
            [ fromLovelace (address "1") 5
            , fromLovelace (address "2") 10
            ]

        context =
            { availableOutputs = availableOutputs
            , alreadySelectedOutputs = []
            , targetAmount = 30
            , changeAddress = address "change"
            }

        result =
            largestFirst context 5
    in
    Expect.equal (Err UTxOBalanceInsufficient) result


singleUtxoSingleOutputEqualValueTest : a -> Expect.Expectation
singleUtxoSingleOutputEqualValueTest _ =
    let
        context =
            { availableOutputs = [ fromLovelace (address "1") 10 ]
            , alreadySelectedOutputs = []
            , targetAmount = 10
            , changeAddress = address "change"
            }

        nmax =
            5

        expectedResult =
            Ok
                { selectedOutputs = context.availableOutputs
                , changeOutput = Nothing
                }
    in
    largestFirst context nmax
        |> Expect.equal expectedResult



-- Fixtures


address : String -> Bytes
address suffix =
    Bytes.fromStringUnchecked <| "addr" ++ suffix
