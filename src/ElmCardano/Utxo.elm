module ElmCardano.Utxo exposing (..)

{-| Handling outputs.
-}

import Bytes exposing (Bytes)
import Cbor.Encode as E
import Cbor.Encode.Extra as E
import Cbor.Tag as Tag
import ElmCardano.Data as Data exposing (Data)
import ElmCardano.Hash as Hash exposing (Blake2b_224, Blake2b_256, Hash)
import ElmCardano.Value as Value exposing (Value)


{-| An input eUTxO for a transaction.
-}
type alias Input =
    { transactionId : Hash Blake2b_256
    , outputIndex : Int
    }


{-| The reference for a eUTxO.
-}
type alias OutputReference =
    Input


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


encodeInput : Input -> E.Encoder
encodeInput =
    E.tuple <|
        E.elems
            >> E.elem Hash.encode .transactionId
            >> E.elem E.int .outputIndex


encodeOutput : Output -> E.Encoder
encodeOutput output =
    case output of
        Legacy fields ->
            E.tuple
                (E.elems
                    >> E.elem E.bytes .address
                    >> E.elem Value.encode .amount
                    >> E.optionalElem Hash.encode .datumHash
                )
                fields

        PostAlonzo fields ->
            E.record E.int
                (E.fields
                    >> E.field 0 E.bytes .address
                    >> E.field 1 Value.encode .value
                    >> E.optionalField 2 encodeDatumOption .datumOption
                    >> E.optionalField 3 (\_ -> Debug.todo "encodeReferenceScript") .referenceScript
                )
                fields


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
                    |> Data.encode
                    |> E.encode
                    |> E.tagged Tag.Cbor E.bytes
                ]
