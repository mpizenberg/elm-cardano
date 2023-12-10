module Cbor.Decode.Extra exposing (natural)

import Bytes.Comparable as Bytes
import Cbor
import Cbor.Decode as D exposing (Decoder)
import Natural exposing (Natural)


natural : Decoder Natural
natural =
    D.bigint
        |> D.andThen
            (\( sign, bytes ) ->
                case sign of
                    Cbor.Positive ->
                        -- TODO: do something more efficient than going through a hex string
                        case Bytes.fromBytes bytes |> Bytes.toString |> Natural.fromHexString of
                            Just nat ->
                                D.succeed nat

                            _ ->
                                D.fail

                    Cbor.Negative ->
                        D.fail
            )
