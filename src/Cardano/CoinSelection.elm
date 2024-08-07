module Cardano.CoinSelection exposing
    ( Context, Error(..), Selection, Algorithm
    , largestFirst
    )

{-| Module `Cardano.CoinSelection` provides functionality for performing
coin selection based on a set of available UTXOs and a set of requested outputs.
It exports functions for sorting UTXOs and performing the Largest-First coin
selection algorithm as described in CIP2 (<https://cips.cardano.org/cips/cip2/>).


# Types

@docs Context, Error, Selection, Algorithm


# Strategies

@docs largestFirst

-}

import Cardano.Utxo as Utxo exposing (Output, OutputReference, compareLovelace, lovelace, totalLovelace)
import Cardano.Value exposing (Value, onlyLovelace)
import Natural as N exposing (Natural)


{-| Enumerates the possible errors that can occur during coin selection.
-}
type Error
    = MaximumInputCountExceeded
    | UTxOBalanceInsufficient


{-| Represents the result of a successful coin selection.

TODO: Also keep OutputReference around because we’ll need it.

-}
type alias Selection =
    { selectedOutputs : List ( OutputReference, Output )
    , change : Maybe Value
    }


{-| Holds the arguments necessary for performing coin selection.

TODO: Also keep OutputReference around because we’ll need it.

-}
type alias Context =
    { availableOutputs : List ( OutputReference, Output )
    , alreadySelectedOutputs : List ( OutputReference, Output )
    , targetAmount : Natural
    }


{-| Alias for the function signature of a utxo selection algorithm.
-}
type alias Algorithm =
    Int -> Context -> Result Error Selection


{-| Implements the Largest-First coin selection algorithm as described in CIP2.

Takes a `Context` record containing the available UTXOs, initially
selected UTXOs, requested outputs, and change address, along with an `Int`
representing the maximum number of inputs allowed. Returns either a
`Error` or a `Selection`. See <https://cips.cardano.org/cips/cip2/#largestfirst>

-}
largestFirst : Algorithm
largestFirst maxInputCount context =
    let
        sortedAvailableUtxo =
            List.sortWith (\( _, o1 ) ( _, o2 ) -> reverseOrder Utxo.compareLovelace o1 o2) context.availableOutputs
    in
    doLargestFirst
        { maxInputCount = maxInputCount
        , selectedInputCount = List.length context.alreadySelectedOutputs
        , accumulatedAmount = totalLovelace (List.map Tuple.second context.alreadySelectedOutputs)
        , targetAmount = context.targetAmount
        , availableOutputs = sortedAvailableUtxo
        , selectedOutputs = context.alreadySelectedOutputs
        }


reverseOrder : (a -> a -> Order) -> a -> a -> Order
reverseOrder f x y =
    f y x



-- doLargestFirst : Int -> Int -> Int -> List Output -> List Output -> Result Error (Bytes -> Selection)


doLargestFirst :
    { maxInputCount : Int
    , selectedInputCount : Int
    , accumulatedAmount : Natural
    , targetAmount : Natural
    , availableOutputs : List ( OutputReference, Output )
    , selectedOutputs : List ( OutputReference, Output )
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
                    , accumulatedAmount = N.add (lovelace <| Tuple.second utxo) accumulatedAmount
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
