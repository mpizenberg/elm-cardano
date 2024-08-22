module Cardano.Utxo exposing
    ( OutputReference, TransactionId, Output, DatumHash, DatumOption(..)
    , RefDict, emptyRefDict, refDictFromList
    , fromLovelace
    , lovelace, totalLovelace, compareLovelace, isAdaOnly
    , minAda, checkMinAda
    , encodeOutputReference, encodeOutput, encodeDatumOption
    , decodeOutputReference, decodeOutput
    )

{-| Handling outputs.


## Definitions

@docs OutputReference, TransactionId, Output, DatumHash, DatumOption


## Dictionary with [OutputReference] keys

@docs RefDict, emptyRefDict, refDictFromList


## Build

@docs fromLovelace


## Query

@docs lovelace, totalLovelace, compareLovelace, isAdaOnly


## Compute

@docs minAda, checkMinAda


## Convert

@docs encodeOutputReference, encodeOutput, encodeDatumOption

@docs decodeOutputReference, decodeOutput

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Address)
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset as MultiAsset exposing (MultiAsset)
import Cardano.Script as Script exposing (Script)
import Cardano.Value as Value exposing (Value)
import Cbor.Decode as D
import Cbor.Decode.Extra as D
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Cbor.Tag as Tag
import Dict.Any exposing (AnyDict)
import Natural as N exposing (Natural)
import Set exposing (Set)


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


{-| Convenience type for `Dict` with [OutputReference] keys.
-}
type alias RefDict a =
    AnyDict ( String, Int ) OutputReference a


{-| Convenience empty initialization for `Dict` with [OutputReference] keys.
-}
emptyRefDict : RefDict a
emptyRefDict =
    Dict.Any.empty (\ref -> ( Bytes.toString ref.transactionId, ref.outputIndex ))


{-| Convenience function to create a `Dict` with [OutputReference] keys from a list.
-}
refDictFromList : List ( OutputReference, a ) -> RefDict a
refDictFromList =
    Dict.Any.fromList (\ref -> ( Bytes.toString ref.transactionId, ref.outputIndex ))


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
type alias Output =
    { address : Address
    , amount : Value
    , datumOption : Maybe DatumOption
    , referenceScript : Maybe Script
    }


{-| Phantom type for 32-bytes datum hashes.
This is a Blake2b-256 hash.
-}
type DatumHash
    = DatumHash_ Never


{-| Compare UTxOs by lovelace value.
-}
compareLovelace : Output -> Output -> Order
compareLovelace a b =
    N.compare (lovelace a) (lovelace b)


{-| Construct an `Output` from an `Address` and a lovelace amount
-}
fromLovelace : Address -> Natural -> Output
fromLovelace address amount =
    { address = address
    , amount = Value.onlyLovelace amount
    , datumOption = Nothing
    , referenceScript = Nothing
    }


{-| Extract the amount of lovelace in an `Output`
-}
lovelace : Output -> Natural
lovelace output =
    output.amount.lovelace


{-| Calculate the total number of lovelace in a collection of `Output`
-}
totalLovelace : List Output -> Natural
totalLovelace =
    List.foldr (\output total -> N.add (lovelace output) total) N.zero


{-| Check if the output contains only Ada.
Nothing else is allowed, no tokens, no datum, no ref script.
-}
isAdaOnly : Output -> Bool
isAdaOnly { amount, datumOption, referenceScript } =
    (amount.assets == MultiAsset.empty)
        && (datumOption == Nothing)
        && (referenceScript == Nothing)


{-| Compute minimum Ada lovelace for a given [Output].

The formula is given by CIP 55,
with current value of `4310` for `coinsPerUTxOByte`.

TODO: provide `coinsPerUTxOByte` in function arguments?

-}
minAda : Output -> Natural
minAda output =
    E.encode (encodeOutput output)
        |> (Bytes.fromBytes >> Bytes.width)
        |> (\w -> N.fromSafeInt ((160 + w) * 4310))


{-| Check that an [Output] has enough ada to cover its size.
-}
checkMinAda : Output -> Result String ()
checkMinAda output =
    let
        outputMinAda =
            minAda output
    in
    if lovelace output |> N.isGreaterThanOrEqual outputMinAda then
        Ok ()

    else
        Err ("Output has less ada than its required min ada (" ++ N.toString outputMinAda ++ "):\n" ++ Debug.toString output)


{-| CBOR encoder for [Output].
-}
encodeOutput : Output -> E.Encoder
encodeOutput output =
    E.record E.int
        (E.fields
            >> E.field 0 Address.toCbor .address
            >> E.field 1 Value.encode .amount
            >> E.optionalField 2 encodeDatumOption .datumOption
            >> E.optionalField 3
                (Script.encodeScript
                    >> E.encode
                    >> E.tagged Tag.Cbor E.bytes
                )
                .referenceScript
        )
        output


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
        preBabbageBuilder address amount optionalDatum =
            { address = address
            , amount = amount
            , datumOption = optionalDatum
            , referenceScript = Nothing
            }

        preBabbage =
            D.tuple preBabbageBuilder <|
                D.elems
                    -- Address
                    >> D.elem Address.decode
                    -- Coin value (lovelace)
                    >> D.elem Value.fromCbor
                    -- ? datum_hash : $hash32
                    >> D.optionalElem (D.map (DatumHash << Bytes.fromBytes) D.bytes)

        postBabbage =
            D.record D.int Output <|
                D.fields
                    -- Address
                    >> D.field 0 Address.decode
                    -- Coin value (lovelace)
                    >> D.field 1 Value.fromCbor
                    -- ? datum_hash : $hash32
                    >> D.optionalField 2 datumOptionFromCbor
                    -- ? 3 : script_ref   ; New; script reference
                    >> D.optionalField 3 decodeScriptRef
    in
    D.oneOf [ preBabbage, postBabbage, D.failWith "Fail to decode output" ]


{-| Decode a doubly CBOR encoded script for the output `script_ref` field.

    script_ref = #6.24(bytes .cbor script)

-}
decodeScriptRef : D.Decoder Script
decodeScriptRef =
    D.tagged Tag.Cbor D.bytes
        |> D.andThen
            (\( _, scriptCbor ) ->
                case D.decode Script.fromCbor scriptCbor of
                    Just script ->
                        D.succeed script

                    Nothing ->
                        D.fail
            )


datumOptionFromCbor : D.Decoder DatumOption
datumOptionFromCbor =
    D.length
        |> D.ignoreThen D.int
        |> D.andThen
            (\tag ->
                case tag of
                    0 ->
                        D.map (DatumHash << Bytes.fromBytes) D.bytes

                    1 ->
                        D.map Datum decodeOutputDatum

                    _ ->
                        D.failWith ("Unknown datum option tag: " ++ String.fromInt tag)
            )


{-| Decode a doubly CBOR encoded plutus data for the output datum.

    data = #6.24(bytes .cbor plutus_data)

-}
decodeOutputDatum : D.Decoder Data
decodeOutputDatum =
    D.tagged Tag.Cbor D.bytes
        |> D.andThen
            (\( _, datumCbor ) ->
                case D.decode Data.fromCbor datumCbor of
                    Just data ->
                        D.succeed data

                    Nothing ->
                        D.fail
            )
