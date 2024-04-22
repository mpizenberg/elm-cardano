module Cardano.Transaction.AuxiliaryData.Metadatum exposing (Metadatum(..), fromCbor, toCbor)

{-|

@docs Metadatum, fromCbor, toCbor

-}

import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Cbor.Decode as D
import Cbor.Decode.Extra as D
import Cbor.Encode as E
import Cbor.Encode.Extra as E


{-| -}
type
    Metadatum
    -- TODO: replace Int with Natural
    = Int Int
    | Bytes (Bytes Any)
    | String String
    | List (List Metadatum)
    | Map (List ( Metadatum, Metadatum ))


{-| -}
toCbor : Metadatum -> E.Encoder
toCbor metadatum =
    case metadatum of
        Int n ->
            E.int n

        Bytes bytes ->
            Bytes.toCbor bytes

        String str ->
            E.string str

        List metadatums ->
            E.ledgerList toCbor metadatums

        Map metadatums ->
            E.ledgerAssociativeList toCbor toCbor metadatums


{-| -}
fromCbor : D.Decoder Metadatum
fromCbor =
    D.failWith "decodeMetadatum (not implemented) failed to decode"
