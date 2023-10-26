module ElmCardano.CoinSelectionTests exposing (..)

import ElmCardano.CoinSelection exposing (largestFirst, CoinSelectionError, CoinSelectionResult)
import ElmCardano.Transaction exposing (Output(..), Value(..))
import Bytes exposing (Bytes)
import Expect
import Test exposing (Test, describe, test)
import Tests exposing (fromString)

suite : Test
suite =
    describe "ElmCardano.CoinSelection"
        [ describe "largestFirst"
            [ test "basic scenario" <| basicScenarioTest
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

        nmax = 5

        changeAddress = fromString "changeAddr"

        result = largestFirst availableUtxo [] requestedOutputs nmax changeAddress
        expectedResult =
            Ok
                { selectedUtxos = [ availableUtxo |> List.head |> Maybe.withDefault (Legacy { address = fromString "addr1", amount = Coin 50, datumHash = Nothing }) ]
                , requestedOutputs = requestedOutputs
                , changeOutput = Just (Legacy { address = changeAddress, amount = Coin 20, datumHash = Nothing })
                }
    in
    Expect.equal expectedResult result
