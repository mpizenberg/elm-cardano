module Cardano.CoinSelectionTests exposing (..)

import Bytes.Comparable as Bytes
import Cardano.Address as Address exposing (Address, NetworkId(..))
import Cardano.CoinSelection as CoinSelection exposing (Error(..), largestFirst)
import Cardano.Utxo exposing (Output, fromLovelace, totalLovelace)
import Cardano.Value exposing (onlyLovelace)
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
        [ describe "largestFirst"
            [ test "basic scenario" <| basicScenarioTest
            , test "no utxos" <| noOutputsTest
            , test "insufficient funds" <| insufficientFundsTest
            , test "single utxo, single output, equal value" <| singleUtxoSingleOutputEqualValueTest
            , test "target zero, already selected output" <| targetZeroAlreadySelectedOutputTest
            , fuzzCoinSelection "coverage of payments" propCoverageOfPayment
            , fuzzCoinSelection "correctness of change" propCorrectnessOfChange
            ]
        ]


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
            , targetAmount = N.fromSafeInt 30
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
            , targetAmount = N.fromSafeInt 30
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
            , targetAmount = N.fromSafeInt 30
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
            , targetAmount = N.fromSafeInt 10
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
            , targetAmount = N.zero
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


output : String -> Int -> Output
output addrSuffix amount =
    fromLovelace (address addrSuffix) (N.fromSafeInt amount)


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
            Fuzz.map2 fromLovelace
                (Fuzz.int |> Fuzz.map (\i -> address <| "_" ++ String.fromInt i))
                Fuzz.Extra.strictPositiveNatural
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
        Fuzz.Extra.natural


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
            totalLovelace selectedOutputs
                -- |> Expect.atLeast context.targetAmount
                |> N.isGreaterThanOrEqual context.targetAmount
                |> Expect.equal True


propCorrectnessOfChange : Int -> CoinSelection.Context -> Expectation
propCorrectnessOfChange maxInputCount context =
    case largestFirst maxInputCount context of
        Err _ ->
            Expect.pass

        Ok { selectedOutputs, change } ->
            let
                changeAmount =
                    Maybe.map .lovelace change
                        |> Maybe.withDefault N.zero
            in
            totalLovelace selectedOutputs
                |> Expect.equal (N.add changeAmount context.targetAmount)
