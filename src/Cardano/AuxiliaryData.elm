module Cardano.AuxiliaryData exposing (AuxiliaryData, fromCbor, toCbor)

{-|

@docs AuxiliaryData, fromCbor, toCbor

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Metadatum as Metadatum exposing (Metadatum)
import Cardano.Script as Script exposing (NativeScript, ScriptCbor)
import Cbor.Decode as D
import Cbor.Decode.Extra as D
import Cbor.Encode as E
import Cbor.Encode.Extra as E
import Cbor.Tag as Tag
import Natural exposing (Natural)


{-| [Transaction] auxiliary data.
-}
type alias AuxiliaryData =
    -- FIXME: Labels can't actually be arbitrarily sized naturals but can only be u64.
    { labels : List ( Natural, Metadatum )
    , nativeScripts : List NativeScript
    , plutusV1Scripts : List (Bytes ScriptCbor)
    , plutusV2Scripts : List (Bytes ScriptCbor)
    , plutusV3Scripts : List (Bytes ScriptCbor)
    }


{-| Encode transaction auxiliary data to CBOR.
-}
toCbor : AuxiliaryData -> E.Encoder
toCbor data =
    data
        |> E.tagged (Tag.Unknown 259)
            (E.record E.int
                (E.fields
                    >> E.optionalField 0 (E.ledgerAssociativeList E.natural Metadatum.toCbor) (nonEmptyList << .labels)
                    >> E.optionalField 1 (E.ledgerList Script.encodeNativeScript) (nonEmptyList << .nativeScripts)
                    >> E.optionalField 2 (E.ledgerList Bytes.toCbor) (nonEmptyList << .plutusV1Scripts)
                    >> E.optionalField 3 (E.ledgerList Bytes.toCbor) (nonEmptyList << .plutusV2Scripts)
                    >> E.optionalField 4 (E.ledgerList Bytes.toCbor) (nonEmptyList << .plutusV3Scripts)
                )
            )


{-| Helper function to convert empty lists to [Nothing].
-}
nonEmptyList : List a -> Maybe (List a)
nonEmptyList list =
    if List.isEmpty list then
        Nothing

    else
        Just list


{-| Decode transaction auxiliary data from CBOR.
-}
fromCbor : D.Decoder AuxiliaryData
fromCbor =
    D.oneOf
        -- Shelley variant
        [ D.map (\labels -> { labels = labels, nativeScripts = [], plutusV1Scripts = [], plutusV2Scripts = [], plutusV3Scripts = [] }) <|
            D.associativeList D.natural Metadatum.fromCbor

        -- Allegra variant
        , D.tuple (\txMetadata auxiliaryScripts -> { labels = txMetadata, nativeScripts = auxiliaryScripts, plutusV1Scripts = [], plutusV2Scripts = [], plutusV3Scripts = [] }) <|
            D.elems
                >> D.elem (D.associativeList D.natural Metadatum.fromCbor)
                >> D.elem (D.list Script.decodeNativeScript)

        -- Allonzo / Babbage variant
        , D.tag
            |> D.andThen
                (\tag ->
                    case tag of
                        Tag.Unknown 259 ->
                            let
                                optionalAuxiliaryData maybeMetadata maybeNativeScripts maybePlutusV1Scripts maybePlutusV2Scripts maybePlutusV3Scripts =
                                    { labels = Maybe.withDefault [] maybeMetadata
                                    , nativeScripts = Maybe.withDefault [] maybeNativeScripts
                                    , plutusV1Scripts = Maybe.withDefault [] maybePlutusV1Scripts
                                    , plutusV2Scripts = Maybe.withDefault [] maybePlutusV2Scripts
                                    , plutusV3Scripts = Maybe.withDefault [] maybePlutusV3Scripts
                                    }
                            in
                            D.record D.int optionalAuxiliaryData <|
                                D.fields
                                    >> D.optionalField 0 (D.associativeList D.natural Metadatum.fromCbor)
                                    >> D.optionalField 1 (D.list Script.decodeNativeScript)
                                    >> D.optionalField 2 (D.list (D.map Bytes.fromBytes D.bytes))
                                    >> D.optionalField 3 (D.list (D.map Bytes.fromBytes D.bytes))
                                    >> D.optionalField 4 (D.list (D.map Bytes.fromBytes D.bytes))

                        _ ->
                            D.fail
                )
        ]
