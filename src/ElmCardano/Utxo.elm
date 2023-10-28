module ElmCardano.Utxo exposing
    ( OutputReference, Output(..), DatumOption(..)
    , encodeOutputReference, encodeOutput, encodeDatumOption
    )

{-| Handling outputs.

@docs OutputReference, Output, DatumOption
@docs encodeOutputReference, encodeOutput, encodeDatumOption

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cbor.Encode as E
import Cbor.Tag as Tag
import ElmCardano.Data as Data exposing (Data)
import ElmCardano.Hash as Hash exposing (Blake2b_224, Blake2b_256, Hash)
import ElmCardano.Value as Value exposing (Value)


{-| The reference for a eUTxO.
-}
type alias OutputReference =
    { transactionId : Hash Blake2b_256
    , outputIndex : Int
    }


{-| The content of a eUTxO.
-}
type Output
    = Legacy
        { address : Bytes
        , amount : Value
        , datumHash : Maybe (Hash Blake2b_256)
        }
    | PostAlonzo
        { address : Bytes
        , value : Value
        , datumOption : Maybe DatumOption
        , referenceScript : Maybe (Hash Blake2b_224)
        }


{-| Nickname for data stored in a eUTxO.
-}
type DatumOption
    = DatumHash (Hash Blake2b_256)
    | Datum Data


{-| CBOR encoder for [OutputReference].
-}
encodeOutputReference : OutputReference -> E.Encoder
encodeOutputReference =
    E.tuple <|
        E.elems
            >> E.elem Hash.encode .transactionId
            >> E.elem E.int .outputIndex


{-| CBOR encoder for [Output].
-}
encodeOutput : Output -> E.Encoder
encodeOutput output =
    case output of
        Legacy fields ->
            E.tuple
                (E.elems
                    >> E.elem Bytes.toCbor .address
                    >> E.elem Value.encode .amount
                    >> E.optionalElem Hash.encode .datumHash
                )
                fields

        PostAlonzo fields ->
            E.record E.int
                (E.fields
                    >> E.field 0 Bytes.toCbor .address
                    >> E.field 1 Value.encode .value
                    >> E.optionalField 2 encodeDatumOption .datumOption
                    >> E.optionalField 3 (\_ -> Debug.todo "encodeReferenceScript") .referenceScript
                )
                fields


{-| CBOR encoder for [DatumOption].
-}
encodeDatumOption : DatumOption -> E.Encoder
encodeDatumOption datumOption =
    E.list identity <|
        case datumOption of
            DatumHash hash ->
                [ E.int 0
                , Hash.encode hash
                ]

            Datum datum ->
                [ E.int 1
                , datum
                    |> Data.toCbor
                    |> E.encode
                    |> E.tagged Tag.Cbor E.bytes
                ]
