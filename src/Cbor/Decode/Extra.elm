module Cbor.Decode.Extra exposing
    ( natural, integer
    , failWith
    )

{-| Extra CBOR decoding utility functions.

@docs natural, integer
@docs failWith

-}

import Bytes.Comparable as Bytes
import Cbor
import Cbor.Decode as D exposing (Decoder)
import Integer exposing (Integer)
import Natural exposing (Natural)


{-| Decode an unbounded positive integer.
-}
natural : Decoder Natural
natural =
    bigNum
        |> D.andThen
            (\( sign, nat ) ->
                case sign of
                    Cbor.Positive ->
                        D.succeed nat

                    Cbor.Negative ->
                        D.fail
            )


{-| Decode an unbounded integer.
-}
integer : Decoder Integer
integer =
    bigNum
        |> D.andThen
            (\( sign, nat ) ->
                case sign of
                    Cbor.Positive ->
                        D.succeed (Integer.fromNatural nat)

                    Cbor.Negative ->
                        D.succeed (Integer.negate <| Integer.fromNatural nat)
            )


bigNum : Decoder ( Cbor.Sign, Natural )
bigNum =
    D.bigint
        |> D.andThen
            (\( sign, bytes ) ->
                -- TODO: do something more efficient than going through a hex string
                case Bytes.fromBytes bytes |> Bytes.toString |> Natural.fromHexString of
                    Just nat ->
                        D.succeed ( sign, nat )

                    _ ->
                        D.fail
            )


{-| Helper decoder to display the raw bytes on which the decoder has failed.
-}
failWith : String -> D.Decoder a
failWith msg =
    D.oneOf [ D.map Bytes.fromBytes D.raw, D.succeed <| Bytes.fromStringUnchecked "..." ]
        |> D.andThen
            (\rawBytes ->
                let
                    _ =
                        Debug.log msg (Bytes.toString rawBytes)
                in
                D.fail
            )
