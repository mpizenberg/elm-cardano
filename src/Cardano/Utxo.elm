module Cardano.Utxo exposing
    ( OutputReference, TransactionId, Output(..), DatumHash, DatumOption(..)
    , fromLovelace
    , lovelace, totalLovelace
    , sortByAscendingLovelace, sortByDescendingLovelace
    , encodeOutputReference, encodeOutput, encodeDatumOption
    , decodeOutputReference, decodeOutput
    )

{-| Handling outputs.


## Definitions

@docs OutputReference, TransactionId, Output, DatumHash, DatumOption


## Build

@docs fromLovelace


## Query

@docs lovelace, totalLovelace


## Transform

@docs sortByAscendingLovelace, sortByDescendingLovelace


## Convert

@docs encodeOutputReference, encodeOutput, encodeDatumOption

@docs decodeOutputReference, decodeOutput

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Address)
import Cardano.Data as Data exposing (Data)
import Cardano.Script as Script exposing (Script)
import Cardano.Value as Value exposing (Value)
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Cbor.Tag as Tag
import Natural as N exposing (Natural)


{-| The reference for a eUTxO.
-}
type alias OutputReference =
    { transactionId : Bytes TransactionId
    , outputIndex : Int
    }


{-| Phantom type for 32-bytes transaction IDs.
This is a Blake2b-256 hash.
-}
type TransactionId
    = TransactionId Never


{-| CBOR encoder for [OutputReference].
-}
encodeOutputReference : OutputReference -> E.Encoder
encodeOutputReference =
    E.tuple <|
        E.elems
            >> E.elem Bytes.toCbor .transactionId
            >> E.elem E.int .outputIndex


{-| Decode an [OutputReference], such as for transaction inputs.
-}
decodeOutputReference : D.Decoder OutputReference
decodeOutputReference =
    D.tuple OutputReference <|
        D.elems
            >> D.elem (D.map Bytes.fromBytes D.bytes)
            >> D.elem D.int


{-| The content of a eUTxO.
-}
type Output
    = Legacy
        { address : Address
        , amount : Value
        , datumHash : Maybe (Bytes DatumHash)
        }
    | PostAlonzo
        { address : Address
        , value : Value
        , datumOption : Maybe DatumOption

        -- TODO: this is wrong, only script_ref here, not the full script
        , referenceScript : Maybe Script
        }


{-| Phantom type for 32-bytes datum hashes.
This is a Blake2b-256 hash.
-}
type DatumHash
    = DatumHash_ Never


{-| Sorts a list of UTXOs in descending order by lovelace value.
-}
sortByDescendingLovelace : List Output -> List Output
sortByDescendingLovelace =
    List.sortWith (\a b -> N.compare (lovelace b) (lovelace a))


{-| Sorts a list of UTXOs in ascending order by lovelace value.
-}
sortByAscendingLovelace : List Output -> List Output
sortByAscendingLovelace =
    List.sortWith (\a b -> N.compare (lovelace a) (lovelace b))


{-| Construct an `Output` from an `Address` and a lovelace amount
-}
fromLovelace : Address -> Natural -> Output
fromLovelace address amount =
    Legacy
        { address = address
        , amount = Value.onlyLovelace amount
        , datumHash = Nothing
        }


{-| Extract the amount of lovelace in an `Output`
-}
lovelace : Output -> Natural
lovelace output =
    case output of
        Legacy legacyOutput ->
            legacyOutput.amount.lovelace

        PostAlonzo postAlonzoOutput ->
            postAlonzoOutput.value.lovelace


{-| Calculate the total number of lovelace in a collection of `Output`
-}
totalLovelace : List Output -> Natural
totalLovelace =
    List.foldr (\output total -> N.add (lovelace output) total) N.zero


{-| CBOR encoder for [Output].
-}
encodeOutput : Output -> E.Encoder
encodeOutput output =
    case output of
        Legacy fields ->
            E.tuple
                (E.elems
                    >> E.elem Address.toCbor .address
                    >> E.elem Value.encode .amount
                    >> E.optionalElem Bytes.toCbor .datumHash
                )
                fields

        PostAlonzo fields ->
            E.record E.int
                (E.fields
                    >> E.field 0 Address.toCbor .address
                    >> E.field 1 Value.encode .value
                    >> E.optionalField 2 encodeDatumOption .datumOption
                    >> E.optionalField 3
                        (Script.encodeScript
                            >> E.encode
                            >> E.tagged Tag.Cbor E.bytes
                        )
                        .referenceScript
                )
                fields


{-| Nickname for data stored in a eUTxO.
-}
type DatumOption
    = DatumHash (Bytes DatumHash)
    | Datum Data


{-| CBOR encoder for [DatumOption].
-}
encodeDatumOption : DatumOption -> E.Encoder
encodeDatumOption datumOption =
    EE.ledgerList identity <|
        case datumOption of
            DatumHash hash ->
                [ E.int 0
                , Bytes.toCbor hash
                ]

            Datum datum ->
                [ E.int 1
                , datum
                    |> Data.toCbor
                    |> E.encode
                    |> E.tagged Tag.Cbor E.bytes
                ]


{-| CBOR decoder for an [Output].
-}
decodeOutput : D.Decoder Output
decodeOutput =
    let
        legacyOutputBuilder address amount optionalDatumHash =
            Legacy
                { address = address
                , amount = amount
                , datumHash = optionalDatumHash
                }
    in
    D.tuple legacyOutputBuilder <|
        D.elems
            -- Address
            >> D.elem Address.decode
            -- Coin value (lovelace)
            >> D.elem Value.fromCbor
            -- ? datum_hash : $hash32
            >> D.optionalElem (D.map Bytes.fromBytes D.bytes)
