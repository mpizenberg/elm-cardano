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

import Bytes.Comparable exposing (Bytes)
import ElmCardano.Utxo
    exposing
        ( Output
        , fromLovelace
        , lovelace
        , sortByDescendingLovelace
        , totalLovelace
        )


{-| Enumerates the possible errors that can occur during coin selection.
-}
type Error
    = MaximumInputCountExceeded
    | UTxOBalanceInsufficient


{-| Represents the result of a successful coin selection.
-}
type alias Selection =
    { selectedOutputs : List Output
    , changeOutput : Maybe Output
    }


{-| Holds the arguments necessary for performing coin selection.
-}
type alias Context =
    { availableOutputs : List Output
    , alreadySelectedOutputs : List Output
    , targetAmount : Int
    , changeAddress : Bytes
    }


{-| Implements the Largest-First coin selection algorithm as described in CIP2.
Takes a `Context` record containing the available UTXOs, initially
selected UTXOs, requested outputs, and change address, along with an `Int`
representing the maximum number of inputs allowed. Returns either a
`Error` or a `Selection`. See <https://cips.cardano.org/cips/cip2/#largestfirst>
-}
largestFirst : Context -> Int -> Result Error Selection
largestFirst args nmax =
    let
        sortedAvailableUtxo =
            sortByDescendingLovelace args.availableOutputs

        remainingAmount =
            args.targetAmount - totalLovelace args.alreadySelectedOutputs
    in
    doLargestFirst nmax remainingAmount sortedAvailableUtxo args.alreadySelectedOutputs
        |> Result.map (\withAddress -> withAddress args.changeAddress)


doLargestFirst : Int -> Int -> List Output -> List Output -> Result Error (Bytes -> Selection)
doLargestFirst nmax remaining available selected =
    if List.length selected > nmax then
        Err MaximumInputCountExceeded

    else if remaining > 0 then
        case available of
            [] ->
                Err UTxOBalanceInsufficient

            utxo :: utxos ->
                doLargestFirst nmax (remaining - lovelace utxo) utxos (utxo :: selected)

    else
        Ok <|
            \changeAddress ->
                { selectedOutputs = selected
                , changeOutput =
                    if remaining == 0 then
                        Nothing

                    else
                        Just (fromLovelace changeAddress -remaining)
                }
