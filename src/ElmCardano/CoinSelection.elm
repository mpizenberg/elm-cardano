module ElmCardano.CoinSelection exposing (..)

{-| Module `ElmCardano.CoinSelection` provides functionality for performing 
coin selection based on a set of available UTXOs and a set of requested outputs. 
It exports functions for sorting UTXOs and performing the Largest-First coin 
selection algorithm as described in CIP2 (https://cips.cardano.org/cips/cip2/).

# Types
@docs CoinSelectionError, CoinSelectionResult, CoinSelectionArgs

# Sorting Functions
@docs sortByDescendingLovelace, sortByAscendingLovelace

# Coin Selection
@docs largestFirst

# Utility Functions
@docs totalValue

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import ElmCardano.Utxo exposing (Output(..), lovelace)
import ElmCardano.Value exposing (onlyLovelace)

{-| Enumerates the possible errors that can occur during coin selection.
-}
type CoinSelectionError
    = MaximumInputCountExceeded
    | UTxOBalanceInsufficient

{-| Represents the result of a successful coin selection.
-}
type alias CoinSelectionResult =
    { selectedUtxos : List Output
    , requestedOutputs : List Output
    , changeOutput : Maybe Output
    }

{-| Holds the arguments necessary for performing coin selection.
-}
type alias CoinSelectionArgs =
    { availableUtxos : List Output
    , selectedUtxos : List Output
    , requestedOutputs : List Output
    , changeAddress : Bytes
    }

{-| Sorts a list of UTXOs in descending order by lovelace value.
-}
sortByDescendingLovelace : List Output -> List Output
sortByDescendingLovelace utxos =
    List.sortWith (\utxo1 utxo2 -> compare (lovelace utxo2) (lovelace utxo1)) utxos

{-| Sorts a list of UTXOs in ascending order by lovelace value.
-}
sortByAscendingLovelace : List Output -> List Output
sortByAscendingLovelace utxos =
    List.sortWith (\utxo1 utxo2 -> compare (lovelace utxo1) (lovelace utxo2)) utxos

{-| Implements the Largest-First coin selection algorithm as described in CIP2.
Takes a `CoinSelectionArgs` record containing the available UTXOs, initially
selected UTXOs, requested outputs, and change address, along with an `Int` 
representing the maximum number of inputs allowed. Returns either a 
`CoinSelectionError` or a `CoinSelectionResult`. See https://cips.cardano.org/cips/cip2/#largestfirst
-}
largestFirst : CoinSelectionArgs -> Int -> Result CoinSelectionError CoinSelectionResult
largestFirst args nmax =
    let
        sortedAvailableUtxo =
            sortByDescendingLovelace args.availableUtxos

        createChangeOutput : Int -> Output
        createChangeOutput amount =
            Legacy
                { address = args.changeAddress
                , amount = onlyLovelace amount
                , datumHash = Nothing
                }

        remainingValue =
            totalValue args.requestedOutputs

        recursiveSelect : Int -> List Output -> List Output -> Result CoinSelectionError CoinSelectionResult
        recursiveSelect remaining available selected =
            if List.length selected > nmax then
                Err MaximumInputCountExceeded

            else if remaining > 0 then
                case available of
                    [] ->
                        Err UTxOBalanceInsufficient

                    utxo :: utxos ->
                        let
                            newRemaining =
                                remaining - lovelace utxo
                        in
                        recursiveSelect newRemaining utxos (utxo :: selected)

            else
                let
                    changeAmount =
                        totalValue selected - totalValue args.requestedOutputs

                    changeOutput =
                        if changeAmount > 0 then
                            Just (createChangeOutput changeAmount)

                        else
                            Nothing
                in
                Ok { selectedUtxos = selected, requestedOutputs = args.requestedOutputs, changeOutput = changeOutput }
    in
    recursiveSelect remainingValue sortedAvailableUtxo args.selectedUtxos


{-| Computes the total value of a list of UTXOs in lovelace.
-}
totalValue : List Output -> Int
totalValue utxos =
    List.foldr (\utxo total -> lovelace utxo + total) 0 utxos
