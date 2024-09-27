module Cbor.Decode.Extra exposing
    ( set
    , natural, integer
    , failWith
    )

{-| Extra CBOR decoding utility functions.

@docs set
@docs natural, integer
@docs failWith

-}

import Bytes.Comparable as Bytes
import Cbor
import Cbor.Decode as D exposing (Decoder)
import Cbor.Tag as Tag
import Integer exposing (Integer)
import Natural exposing (Natural)


{-| Decoder for a set of value. Either an array or within a tag 258.

Don’t ask me why this exists. IOHK designed this standard long ago and wanted to use it.
Regardless of what the people wanted.

Ref: <https://github.com/input-output-hk/cbor-sets-spec/blob/master/CBOR_SETS.md>

-}
set : Decoder a -> Decoder (List a)
set elemDecoder =
    D.oneOf
        [ D.list elemDecoder
        , D.tagged (Tag.Unknown 258) (D.list elemDecoder)
            |> D.map Tuple.second
        ]


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
    D.oneOf [ D.map Bytes.fromBytes D.raw, D.succeed <| Bytes.fromHexUnchecked "..." ]
        |> D.andThen
            (\rawBytes ->
                let
                    _ =
                        Debug.log msg (Bytes.toString rawBytes)
                in
                D.fail
            )
