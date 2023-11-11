module ElmCardano.CoinSelectionTests exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import ElmCardano.Address exposing (Address)
import ElmCardano.CoinSelection as CoinSelection exposing (Error(..), largestFirst)
import ElmCardano.Utxo exposing (fromLovelace, lovelace, totalLovelace)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
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
            , fuzzCoinSelection "coverage of payments" propCoverageOfPayment
            , fuzzCoinSelection "correctness of change" propCorrectnessOfChange
            ]
        ]


basicScenarioTest : a -> Expectation
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

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedOutputs = [ fromLovelace (address "1") 50 ]
                , changeOutput = Just <| fromLovelace context.changeAddress 20
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
            , targetAmount = 30
            , changeAddress = address "change"
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
            largestFirst 5 context
    in
    Expect.equal (Err UTxOBalanceInsufficient) result


singleUtxoSingleOutputEqualValueTest : a -> Expectation
singleUtxoSingleOutputEqualValueTest _ =
    let
        context =
            { availableOutputs = [ fromLovelace (address "1") 10 ]
            , alreadySelectedOutputs = []
            , targetAmount = 10
            , changeAddress = address "change"
            }

        maxInputCount =
            5

        expectedResult =
            Ok
                { selectedOutputs = context.availableOutputs
                , changeOutput = Nothing
                }
    in
    largestFirst maxInputCount context
        |> Expect.equal expectedResult



-- Fixtures


address : String -> Bytes Address
address suffix =
    Bytes.fromStringUnchecked <| "addr" ++ suffix



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
        maxInt =
            100

        outputFuzzer =
            Fuzz.map2 fromLovelace
                (Fuzz.int |> Fuzz.map (\i -> address <| "_" ++ String.fromInt i))
                (Fuzz.intRange 1 maxInt)
    in
    Fuzz.map4 CoinSelection.Context
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
        (Fuzz.intRange 0 maxInt)
        (Fuzz.constant <| address "change")


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
            totalLovelace selectedOutputs |> Expect.atLeast context.targetAmount


propCorrectnessOfChange : Int -> CoinSelection.Context -> Expectation
propCorrectnessOfChange maxInputCount context =
    case largestFirst maxInputCount context of
        Err _ ->
            Expect.pass

        Ok { selectedOutputs, changeOutput } ->
            let
                change =
                    changeOutput
                        |> Maybe.map lovelace
                        |> Maybe.withDefault 0
            in
            totalLovelace selectedOutputs
                |> Expect.equal (change + context.targetAmount)
