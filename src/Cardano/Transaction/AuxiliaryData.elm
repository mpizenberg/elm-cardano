module Cardano.Transaction.AuxiliaryData exposing (AuxiliaryData, fromCbor, toCbor)

{-|

@docs AuxiliaryData, fromCbor, toCbor

-}

import Cardano.Script as Script exposing (NativeScript, PlutusScript, PlutusVersion(..))
import Cardano.Transaction.AuxiliaryData.Metadatum as Metadatum exposing (Metadatum)
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
    , plutusScripts : List ( PlutusVersion, PlutusScript )
    }


{-| Encode transaction auxiliary data to CBOR.
-}
toCbor : AuxiliaryData -> E.Encoder
toCbor data =
    let
        plutusV1Scripts =
            .plutusScripts
                >> List.filterMap
                    (\( version, script ) ->
                        case version of
                            PlutusV1 ->
                                Just script

                            PlutusV2 ->
                                Nothing
                    )

        plutusV2Scripts =
            .plutusScripts
                >> List.filterMap
                    (\( version, script ) ->
                        case version of
                            PlutusV1 ->
                                Nothing

                            PlutusV2 ->
                                Just script
                    )
    in
    data
        |> E.tagged (Tag.Unknown 259)
            (E.record E.int
                (E.fields
                    >> E.field 0 (E.ledgerAssociativeList E.natural Metadatum.toCbor) .labels
                    >> E.field 1 (E.ledgerList Script.encodeNativeScript) .nativeScripts
                    >> E.field 2 (E.ledgerList Script.encodePlutusScript) plutusV1Scripts
                    >> E.field 3 (E.ledgerList Script.encodePlutusScript) plutusV2Scripts
                )
            )


{-| Decode transaction auxiliary data from CBOR.
-}
fromCbor : D.Decoder AuxiliaryData
fromCbor =
    -- TODO: This only holds for Shelley. The format has changed in Allegra and then Alonzo.
    D.map (\labels -> { labels = labels, nativeScripts = [], plutusScripts = [] }) <|
        D.associativeList D.natural Metadatum.fromCbor
