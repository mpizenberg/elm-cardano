module ElmCardano.CoinSelection exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import ElmCardano.Utxo exposing (Output(..), lovelace)
import ElmCardano.Value exposing (onlyLovelace)


type CoinSelectionError
    = MaximumInputCountExceeded
    | UTxOBalanceInsufficient


type alias CoinSelectionResult =
    { selectedUtxos : List Output
    , requestedOutputs : List Output
    , changeOutput : Maybe Output
    }


type alias CoinSelectionArgs =
    { availableUtxo : List Output
    , selectedUtxos : List Output
    , requestedOutputs : List Output
    , changeAddress : Bytes
    }


sortByDescendingLovelace : List Output -> List Output
sortByDescendingLovelace utxos =
    List.sortWith (\utxo1 utxo2 -> compare (lovelace utxo2) (lovelace utxo1)) utxos


sortByAscendingLovelace : List Output -> List Output
sortByAscendingLovelace utxos =
    List.sortWith (\utxo1 utxo2 -> compare (lovelace utxo1) (lovelace utxo2)) utxos



-- https://cips.cardano.org/cips/cip2/#largestfirst


largestFirst : CoinSelectionArgs -> Int -> Result CoinSelectionError CoinSelectionResult
largestFirst args nmax =
    let
        sortedAvailableUtxo =
            sortByDescendingLovelace args.availableUtxo

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


totalValue : List Output -> Int
totalValue utxos =
    List.foldr (\utxo total -> lovelace utxo + total) 0 utxos
