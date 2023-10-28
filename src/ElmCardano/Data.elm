module ElmCardano.Data exposing (Data(..), collectCborItems, collectCborPairs, fromCbor, fromCborItem, toCbor)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cbor exposing (CborItem(..))
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Tag as Tag


{-| A Data is an opaque compound type that can represent any possible user-defined type in Aiken.
-}
type Data
    = Constr Int (List Data)
    | Map (List ( Data, Data ))
    | List (List Data)
    | Int Int
    | Bytes Bytes


toCbor : Data -> E.Encoder
toCbor data =
    let
        -- NOTE: 'Data' lists are weirdly encoded:
        --
        -- 1. They are encoded as definite empty lists (0x80)
        -- 2. But, are encoded as indefinite list otherwise.
        encodeList : List Data -> E.Encoder
        encodeList xs =
            case xs of
                [] ->
                    E.length 0

                _ ->
                    E.indefiniteList toCbor xs
    in
    case data of
        Constr ix fields ->
            if 0 <= ix && ix < 7 then
                E.tagged (Tag.Unknown <| 121 + ix) encodeList fields

            else if 7 <= ix && ix < 128 then
                E.tagged (Tag.Unknown <| 1280 + ix - 7) encodeList fields

            else
                E.tagged (Tag.Unknown 102)
                    (E.tuple <|
                        E.elems
                            >> E.elem E.int .ix
                            >> E.elem encodeList .fields
                    )
                    { ix = ix, fields = fields }

        Map xs ->
            E.associativeList toCbor toCbor xs

        List xs ->
            encodeList xs

        -- NOTE: Technically, Plutus allows to encode arbitrarily large
        -- integers. It tries to encode them as CBOR basic int when possible,
        -- and otherwise default to bytes tagged as Positive or Negative
        -- 'BigNum'.
        --
        -- Yet in Elm / JavaScript, we only truly support ints in the range of
        -- -2^53, 2^53-1; which is well within the values that can be encoded
        -- as plain CBOR int.
        --
        -- Similarly for decoding, we cannot decode larger ints value _anyway_,
        -- unless we start using a BigInt library. For the purpose of this
        -- particular SDK, we currently make the choice of simply not supporting
        -- large ints. We may revise that choice if a use-case is made.
        Int i ->
            E.int i

        Bytes bytes ->
            if Bytes.width bytes <= 64 then
                E.bytes (Bytes.toBytes bytes)

            else
                E.sequence <|
                    E.beginBytes
                        :: List.foldr
                            (\chunk rest -> E.bytes (Bytes.toBytes chunk) :: rest)
                            [ E.break ]
                            (Bytes.chunksOf 64 bytes)


fromCbor : D.Decoder Data
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


fromCborItem : CborItem -> Maybe Data
fromCborItem item =
    case item of
        CborMap xs ->
            collectCborPairs [] xs |> Maybe.map Map

        CborList xs ->
            collectCborItems [] xs |> Maybe.map List

        CborInt i ->
            Just (Int i)

        CborBytes bs ->
            Just (Bytes <| Bytes.fromBytes bs)

        CborTag (Tag.Unknown n) tagged ->
            if n == 102 then
                case tagged of
                    CborList [ CborInt ix, CborList fields ] ->
                        collectCborItems [] fields
                            |> Maybe.map (Constr ix)

                    _ ->
                        Nothing

            else
                case tagged of
                    CborList fields ->
                        let
                            ix =
                                if n >= 1280 then
                                    n - 1280 + 7

                                else
                                    n - 121
                        in
                        collectCborItems [] fields
                            |> Maybe.map (Constr ix)

                    _ ->
                        Nothing

        _ ->
            Nothing


collectCborPairs : List ( Data, Data ) -> List ( CborItem, CborItem ) -> Maybe (List ( Data, Data ))
collectCborPairs st pairs =
    case pairs of
        [] ->
            Just (List.reverse st)

        ( left, right ) :: tail ->
            fromCborItem left
                |> Maybe.andThen
                    (\l ->
                        fromCborItem right
                            |> Maybe.andThen
                                (\r ->
                                    collectCborPairs (( l, r ) :: st) tail
                                )
                    )


collectCborItems : List Data -> List CborItem -> Maybe (List Data)
collectCborItems st items =
    case items of
        [] ->
            Just (List.reverse st)

        head :: tail ->
            fromCborItem head
                |> Maybe.andThen (\s -> collectCborItems (s :: st) tail)
