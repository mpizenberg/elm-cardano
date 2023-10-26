module ElmCardano.CoinSelection exposing (..)

import ElmCardano.Transaction exposing (Output(..), Value(..))
import Bytes exposing (Bytes)

type CoinSelectionError
    = MaximumInputCountExceeded
    | UTxOBalanceInsufficient

type alias CoinSelectionResult =
    { selectedUtxos : List Output
    , requestedOutputs : List Output
    , changeOutput : Maybe Output
    }

-- https://cips.cardano.org/cips/cip2/#largestfirst
largestFirst :
    List Output
    -> List Output
    -> List Output
    -> Int
    -> Bytes 
    -> Result CoinSelectionError CoinSelectionResult
largestFirst availableUtxo selectedUtxos requestedOutputs nmax changeAddress =
    let
        -- Sort the availableUtxo list once before entering the recursive function.
        sortedAvailableUtxo =
            List.sortWith (\utxo1 utxo2 -> compare (getValue utxo2) (getValue utxo1)) availableUtxo

        createChangeOutput : Int -> Output
        createChangeOutput amount =
            -- Here, you'll create an Output with the changeAddress and the amount of change.
            -- Assuming the change is always sent as a Legacy output (adjust as needed):
            Legacy { address = changeAddress, amount = Coin amount, datumHash = Nothing }

        -- Define a nested recursive function to encapsulate the recurring logic.
        recursiveSelect : List Output -> List Output -> Result CoinSelectionError CoinSelectionResult
        recursiveSelect available selected =
            if List.length selected > nmax then
                Err MaximumInputCountExceeded
            else if totalValue selected < totalValue requestedOutputs then
                case List.head available of
                    Just utxo ->
                        let
                            newAvailable = List.drop 1 available
                        in
                            recursiveSelect newAvailable (utxo :: selected)
                        
                    Nothing ->
                        Err UTxOBalanceInsufficient  -- Error termination when available UTXOs are exhausted.
            else
                let
                    totalChange = totalValue selected - totalValue requestedOutputs
                    changeOutput = if totalChange > 0 then Just (createChangeOutput totalChange) else Nothing
                in
                    Ok { selectedUtxos = selected, requestedOutputs = requestedOutputs, changeOutput = changeOutput }
    in
    recursiveSelect sortedAvailableUtxo selectedUtxos

getValue : Output -> Int
getValue output =
    case output of
        Legacy legacyOutput ->
            valueToInt legacyOutput.amount

        PostAlonzo postAlonzoOutput ->
            valueToInt postAlonzoOutput.value

valueToInt : Value -> Int
valueToInt value =
    case value of
        Coin coin ->
            coin

        Multiasset coin _ ->
            coin

totalValue : List Output -> Int
totalValue utxos =
    List.sum (List.map getValue utxos)