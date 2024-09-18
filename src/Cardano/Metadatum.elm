module Cardano.Metadatum exposing (Metadatum(..), fromCbor, toCbor)

{-|

@docs Metadatum, fromCbor, toCbor

-}

import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Cbor exposing (CborItem(..))
import Cbor.Decode as D
import Cbor.Decode.Extra as D
import Cbor.Encode as E
import Cbor.Encode.Extra as E
import Cbor.Tag as Tag
import Integer exposing (Integer)


{-| Transaction auxiliary data metadatum.
-}
type Metadatum
    = Int Integer
    | Bytes (Bytes Any)
    | String String
    | List (List Metadatum)
    | Map (List ( Metadatum, Metadatum ))


{-| Encode Metadatum to CBOR.
-}
toCbor : Metadatum -> E.Encoder
toCbor metadatum =
    case metadatum of
        Int n ->
            E.integer n

        Bytes bytes ->
            Bytes.toCbor bytes

        String str ->
            E.string str

        List metadatums ->
            E.ledgerList toCbor metadatums

        Map metadatums ->
            E.ledgerAssociativeList toCbor toCbor metadatums


{-| Decode Metadatum from CBOR
-}
fromCbor : D.Decoder Metadatum
fromCbor =
    D.any
        |> D.andThen
            (\any ->
                case fromCborItem any of
                    Nothing ->
                        D.fail

                    Just data ->
                        D.succeed data
            )


fromCborItem : CborItem -> Maybe Metadatum
fromCborItem item =
    case item of
        CborMap xs ->
            collectCborPairs [] xs |> Maybe.map Map

        CborList xs ->
            collectCborItems [] xs |> Maybe.map List

        CborBytes bs ->
            Just (Bytes <| Bytes.fromBytes bs)

        CborString str ->
            Just (String str)

        CborInt32 i ->
            Just (Int (Integer.fromSafeInt i))

        CborInt64 ( msb, lsb ) ->
            let
                bigMsb =
                    Integer.fromSafeInt msb
                        |> Integer.mul (Integer.fromSafeInt 4294967296)

                bigLsb =
                    Integer.fromSafeInt lsb
            in
            if msb >= 0 then
                Just (Int (Integer.add bigMsb bigLsb))

            else
                Just (Int (Integer.sub bigMsb bigLsb))

        CborTag Tag.PositiveBigNum _ ->
            E.encode (E.any item)
                |> D.decode D.integer
                |> Maybe.map Int

        CborTag Tag.NegativeBigNum _ ->
            E.encode (E.any item)
                |> D.decode D.integer
                |> Maybe.map Int

        _ ->
            Nothing


collectCborPairs : List ( Metadatum, Metadatum ) -> List ( CborItem, CborItem ) -> Maybe (List ( Metadatum, Metadatum ))
collectCborPairs st pairs =
    case pairs of
        [] ->
            Just (List.reverse st)

        ( left, right ) :: tail ->
            case ( fromCborItem left, fromCborItem right ) of
                ( Just l, Just r ) ->
                    collectCborPairs (( l, r ) :: st) tail

                _ ->
                    Nothing


collectCborItems : List Metadatum -> List CborItem -> Maybe (List Metadatum)
collectCborItems st items =
    case items of
        [] ->
            Just (List.reverse st)

        head :: tail ->
            case fromCborItem head of
                Just s ->
                    collectCborItems (s :: st) tail

                Nothing ->
                    Nothing
