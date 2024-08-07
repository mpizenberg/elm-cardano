module Cardano.CoinSelectionTests exposing (..)

import Bytes.Comparable as Bytes
import Cardano.Address as Address exposing (Address, NetworkId(..))
import Cardano.CoinSelection as CoinSelection exposing (Error(..), largestFirst)
import Cardano.Utxo exposing (Output, OutputReference, fromLovelace, totalLovelace)
import Cardano.Value as Value exposing (Value, onlyLovelace)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Extra
import Natural as N
import Result.Extra as Result
import Test exposing (Test, describe, expectDistribution, fuzzWith, test)
import Test.Distribution as Distribution


suite : Test
suite =
    describe "CoinSelection"
        -- Ada only tests
        [ describe "largestFirst ada only"
            [ test "basic scenario" <| basicScenarioTest
            , test "no utxos" <| noOutputsTest
            , test "insufficient funds" <| insufficientFundsTest
            , test "single utxo, single output, equal value" <| singleUtxoSingleOutputEqualValueTest
            , test "target zero, already selected output" <| targetZeroAlreadySelectedOutputTest
            , fuzzCoinSelection "coverage of payments" propCoverageOfPayment
            , fuzzCoinSelection "correctness of change" propCorrectnessOfChange
            ]

        -- MultiAsset tests
        , describe "largestFirst MultiAsset"
            [ test "basic scenario" <| basicScenarioMultiAssetTest
            , test "no utxos" <| noOutputsMultiAssetTest
            , test "insufficient funds" <| insufficientFundsMultiAssetTest
            , test "single utxo, single output, equal value" <| singleUtxoSingleOutputEqualValueMultiAssetTest
            , test "target zero, already selected output" <| targetZeroAlreadySelectedOutputMultiAssetTest
            ]
        ]



-- Ada only


basicScenarioTest : a -> Expectation
basicScenarioTest _ =
    let
        context =
            { availableOutputs =
                [ output "1" 50
                , output "2" 30
                , output "3" 20
                ]
            , alreadySelectedOutputs = []
            , targetAmount = onlyLovelace <| N.fromSafeInt 30
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedOutputs = [ output "1" 50 ]
                , change = Just <| onlyLovelace (N.fromSafeInt 20)
                }
    in
    largestFirst maxInputCount context
        |> Expect.equal expectedResult


noOutputsTest : a -> Expectation
noOutputsTest _ =
    let
        context =
            { availableOutputs = []
            , alreadySelectedOutputs = []
            , targetAmount = onlyLovelace <| N.fromSafeInt 30
            }

        maxInputCount =
            5
    in
    largestFirst maxInputCount context
        |> Expect.equal (Err UTxOBalanceInsufficient)


insufficientFundsTest : a -> Expectation
insufficientFundsTest _ =
    let
        availableOutputs =
            [ output "1" 5
            , output "2" 10
            ]

        context =
            { availableOutputs = availableOutputs
            , alreadySelectedOutputs = []
            , targetAmount = onlyLovelace <| N.fromSafeInt 30
            }

        result =
            largestFirst 5 context
    in
    Expect.equal (Err UTxOBalanceInsufficient) result


singleUtxoSingleOutputEqualValueTest : a -> Expectation
singleUtxoSingleOutputEqualValueTest _ =
    let
        context =
            { availableOutputs = [ output "1" 10 ]
            , alreadySelectedOutputs = []
            , targetAmount = onlyLovelace <| N.fromSafeInt 10
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedOutputs = context.availableOutputs
                , change = Nothing
                }
    in
    largestFirst maxInputCount context
        |> Expect.equal expectedResult


targetZeroAlreadySelectedOutputTest : a -> Expectation
targetZeroAlreadySelectedOutputTest _ =
    let
        context =
            { availableOutputs = []
            , alreadySelectedOutputs = [ output "1" 1 ]
            , targetAmount = Value.zero
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedOutputs = [ output "1" 1 ]
                , change = Just <| onlyLovelace (N.fromSafeInt 1)
                }
    in
    largestFirst maxInputCount context
        |> Expect.equal expectedResult



-- Fixtures


output : String -> Int -> ( OutputReference, Output )
output addrSuffix amount =
    ( OutputReference (Bytes.fromStringUnchecked "addrSuffix") 0
    , fromLovelace (address addrSuffix) (N.fromSafeInt amount)
    )


address : String -> Address
address suffix =
    Bytes.fromStringUnchecked ("addr" ++ suffix)
        |> Address.enterprise Testnet



-- Fuzzer


fuzzCoinSelection : String -> (Int -> CoinSelection.Context -> Expectation) -> Test
fuzzCoinSelection title prop =
    let
        maxInputCount =
            5
    in
    fuzzWith
        { runs = 100
        , distribution = contextDistribution maxInputCount
        }
        (contextFuzzer maxInputCount)
        title
        (prop maxInputCount)


contextFuzzer : Int -> Fuzzer CoinSelection.Context
contextFuzzer maxInputCount =
    let
        outputFuzzer =
            Fuzz.map2 output
                (Fuzz.map String.fromInt Fuzz.int)
                (Fuzz.intAtLeast 1)
    in
    Fuzz.map3 CoinSelection.Context
        (Fuzz.frequency
            [ ( 1, Fuzz.constant [] )
            , ( 9, Fuzz.listOfLengthBetween 1 (maxInputCount + 1) outputFuzzer )
            ]
        )
        (Fuzz.frequency
            [ ( 1, Fuzz.listOfLengthBetween 0 maxInputCount outputFuzzer )
            , ( 9, Fuzz.constant [] )
            ]
        )
        (Fuzz.map onlyLovelace Fuzz.Extra.natural)


contextDistribution : Int -> Test.Distribution CoinSelection.Context
contextDistribution maxInputCount =
    expectDistribution
        [ ( Distribution.atLeast 70
          , "success"
          , \ctx -> largestFirst maxInputCount ctx |> Result.isOk
          )
        , ( Distribution.atLeast 80
          , "no already selected outputs"
          , \ctx -> ctx.alreadySelectedOutputs |> List.isEmpty
          )
        , ( Distribution.atLeast 5
          , "already selected outputs"
          , \ctx -> ctx.alreadySelectedOutputs |> List.isEmpty |> not
          )
        ]



-- Properties


propCoverageOfPayment : Int -> CoinSelection.Context -> Expectation
propCoverageOfPayment maxInputCount context =
    case largestFirst maxInputCount context of
        Err _ ->
            Expect.pass

        Ok { selectedOutputs } ->
            Value.sum (List.map (Tuple.second >> .amount) selectedOutputs)
                -- |> Expect.atLeast context.targetAmount
                |> Value.atLeast context.targetAmount
                |> Expect.equal True


propCorrectnessOfChange : Int -> CoinSelection.Context -> Expectation
propCorrectnessOfChange maxInputCount context =
    case largestFirst maxInputCount context of
        Err _ ->
            Expect.pass

        Ok { selectedOutputs, change } ->
            let
                changeAmount =
                    Maybe.withDefault Value.zero change
            in
            Value.sum (List.map (Tuple.second >> .amount) selectedOutputs)
                |> Expect.equal (Value.add changeAmount context.targetAmount)



-- MultiAsset


basicScenarioMultiAssetTest : a -> Expectation
basicScenarioMultiAssetTest _ =
    let
        context =
            { availableOutputs =
                [ asset "1" "policy" "name" 30
                , asset "2" "policy" "name" 20
                , asset "3" "policy" "name" 70
                , asset "4" "policy" "name" 10
                ]
            , alreadySelectedOutputs = []
            , targetAmount = token "policy" "name" 30
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedOutputs = [ asset "3" "policy" "name" 70 ]
                , change = Just (token "policy" "name" 40)
                }
    in
    largestFirst maxInputCount context
        |> Expect.equal expectedResult


noOutputsMultiAssetTest : a -> Expectation
noOutputsMultiAssetTest _ =
    let
        context =
            { availableOutputs = []
            , alreadySelectedOutputs = []
            , targetAmount = token "policy" "name" 30
            }

        maxInputCount =
            5
    in
    largestFirst maxInputCount context
        |> Expect.equal (Err UTxOBalanceInsufficient)


insufficientFundsMultiAssetTest : a -> Expectation
insufficientFundsMultiAssetTest _ =
    let
        availableOutputs =
            [ asset "1" "policy" "name" 5
            , asset "2" "policy" "name" 10
            ]

        context =
            { availableOutputs = availableOutputs
            , alreadySelectedOutputs = []
            , targetAmount = token "policy" "name" 30
            }

        result =
            largestFirst 5 context
    in
    Expect.equal (Err UTxOBalanceInsufficient) result


singleUtxoSingleOutputEqualValueMultiAssetTest : a -> Expectation
singleUtxoSingleOutputEqualValueMultiAssetTest _ =
    let
        context =
            { availableOutputs = [ asset "1" "policy" "name" 10 ]
            , alreadySelectedOutputs = []
            , targetAmount = token "policy" "name" 10
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedOutputs = context.availableOutputs
                , change = Nothing
                }
    in
    largestFirst maxInputCount context
        |> Expect.equal expectedResult


targetZeroAlreadySelectedOutputMultiAssetTest : a -> Expectation
targetZeroAlreadySelectedOutputMultiAssetTest _ =
    let
        context =
            { availableOutputs = []
            , alreadySelectedOutputs = [ asset "1" "policy" "name" 1 ]
            , targetAmount = Value.zero
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedOutputs = [ asset "1" "policy" "name" 1 ]
                , change = Just <| token "policy" "name" 1
                }
    in
    largestFirst maxInputCount context
        |> Expect.equal expectedResult



-- Helper functions


asset : String -> String -> String -> Int -> ( OutputReference, Output )
asset addrSuffix policyId name amount =
    ( OutputReference (Bytes.fromStringUnchecked <| "Tx" ++ addrSuffix) 0
    , { address = address addrSuffix
      , amount = token policyId name amount
      , datumOption = Nothing
      , referenceScript = Nothing
      }
    )


token : String -> String -> Int -> Value
token policyId name amount =
    Value.onlyToken (Bytes.fromStringUnchecked policyId) (Bytes.fromStringUnchecked name) (N.fromSafeInt amount)
