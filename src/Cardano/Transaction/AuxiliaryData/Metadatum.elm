module Cardano.Transaction.AuxiliaryData.Metadatum exposing
    ( Metadatum(..)
    , fromCbor
    , jsonDecoder
    , toCbor
    , toJson
    )

import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Cbor.Decode as D
import Cbor.Decode.Extra as D
import Cbor.Encode as E
import Cbor.Encode.Extra as E
import Json.Decode as JD
import Json.Encode as JE


{-| -}
type
    Metadatum
    -- TODO: replace Int with Natural
    = Int Int
    | Bytes (Bytes Any)
    | String String
    | List (List Metadatum)
    | Map (List ( Metadatum, Metadatum ))


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


fromCbor : D.Decoder Metadatum
fromCbor =
    D.failWith "decodeMetadatum (not implemented) failed to decode"


toJson : Metadatum -> JE.Value
toJson metadatum =
    case metadatum of
        Int n ->
            JE.int n

        Bytes bytes ->
            JE.string <| Bytes.toString bytes

        String str ->
            JE.string str

        List metadatums ->
            JE.list toJson metadatums

        Map metadatums ->
            JE.object <|
                List.foldr
                    -- TODO: Keys here are turning into strings, which doesn't
                    -- seem right for `List` and `Map` constructors. But what
                    -- is the right encoding?
                    (\( k, v ) acc -> ( toJson k |> JE.encode 0, toJson v ) :: acc)
                    []
                    metadatums


jsonDecoder : JD.Decoder Metadatum
jsonDecoder =
    JD.oneOf
        [ JD.int
            |> JD.map Int
        , JD.string
            |> JD.map
                (\str ->
                    case Bytes.fromString str of
                        Just bs ->
                            Bytes bs

                        Nothing ->
                            String str
                )
        , JD.list (JD.lazy (\_ -> jsonDecoder))
            |> JD.map List
        , JD.keyValuePairs (JD.lazy (\_ -> jsonDecoder))
            |> JD.map
                (List.filterMap
                    (\( k, v ) ->
                        fromString k
                            |> Maybe.map (\decodedK -> ( decodedK, v ))
                    )
                )
            |> JD.map Map
        ]


fromString : String -> Maybe Metadatum
fromString =
    JD.decodeString (JD.lazy (\_ -> jsonDecoder)) >> Result.toMaybe
