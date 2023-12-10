module ElmCardano.CoinSelection exposing
    ( Context, Error(..), Selection
    , largestFirst
    )

{-| Module `ElmCardano.CoinSelection` provides functionality for performing
coin selection based on a set of available UTXOs and a set of requested outputs.
It exports functions for sorting UTXOs and performing the Largest-First coin
selection algorithm as described in CIP2 (<https://cips.cardano.org/cips/cip2/>).


# Types

@docs Context, Error, Selection


# Strategies

@docs largestFirst

-}

import ElmCardano.Utxo exposing (Output, lovelace, sortByDescendingLovelace, totalLovelace)
import ElmCardano.Value exposing (Value, onlyLovelace)
import Natural as N exposing (Natural)


{-| Enumerates the possible errors that can occur during coin selection.
-}
type Error
    = MaximumInputCountExceeded
    | UTxOBalanceInsufficient


{-| Represents the result of a successful coin selection.
-}
type alias Selection =
    { selectedOutputs : List Output
    , change : Maybe Value
    }


{-| Holds the arguments necessary for performing coin selection.
-}
type alias Context =
    { availableOutputs : List Output
    , alreadySelectedOutputs : List Output
    , targetAmount : Natural
    }


{-| Implements the Largest-First coin selection algorithm as described in CIP2.

Takes a `Context` record containing the available UTXOs, initially
selected UTXOs, requested outputs, and change address, along with an `Int`
representing the maximum number of inputs allowed. Returns either a
`Error` or a `Selection`. See <https://cips.cardano.org/cips/cip2/#largestfirst>

-}
largestFirst : Int -> Context -> Result Error Selection
largestFirst maxInputCount context =
    let
        sortedAvailableUtxo =
            sortByDescendingLovelace context.availableOutputs
    in
    doLargestFirst
        { maxInputCount = maxInputCount
        , selectedInputCount = List.length context.alreadySelectedOutputs
        , accumulatedAmount = totalLovelace context.alreadySelectedOutputs
        , targetAmount = context.targetAmount
        , availableOutputs = sortedAvailableUtxo
        , selectedOutputs = context.alreadySelectedOutputs
        }



-- doLargestFirst : Int -> Int -> Int -> List Output -> List Output -> Result Error (Bytes -> Selection)


doLargestFirst :
    { maxInputCount : Int
    , selectedInputCount : Int
    , accumulatedAmount : Natural
    , targetAmount : Natural
    , availableOutputs : List Output
    , selectedOutputs : List Output
    }
    -> Result Error Selection
doLargestFirst { maxInputCount, selectedInputCount, accumulatedAmount, targetAmount, availableOutputs, selectedOutputs } =
    if selectedInputCount > maxInputCount then
        Err MaximumInputCountExceeded

    else if accumulatedAmount |> N.isLessThan targetAmount then
        case availableOutputs of
            [] ->
                Err UTxOBalanceInsufficient

            utxo :: utxos ->
                doLargestFirst
                    { maxInputCount = maxInputCount
                    , selectedInputCount = selectedInputCount + 1
                    , accumulatedAmount = N.add (lovelace utxo) accumulatedAmount
                    , targetAmount = targetAmount
                    , availableOutputs = utxos
                    , selectedOutputs = utxo :: selectedOutputs
                    }

    else
        Ok
            { selectedOutputs = selectedOutputs
            , change =
                if accumulatedAmount == targetAmount then
                    Nothing

                else
                    Just (onlyLovelace <| N.sub accumulatedAmount targetAmount)
            }
