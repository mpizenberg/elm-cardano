module ElmCardano.Data exposing (..)

import Bitwise exposing (shiftRightBy)
import Bytes exposing (Bytes)
import Bytes.Encode as BE
import Bytes.Extra as Bytes
import Cbor.Encode as E
import Cbor.Encode.Extra as E
import Cbor.Tag as Tag
import Hex.Convert as Hex


{-| A Data is an opaque compound type that can represent any possible user-defined type in Aiken.
-}
type Data
    = Constr Int (List Data)
    | Map (List ( Data, Data ))
    | List (List Data)
    | Int Int
    | HexBytes String


encode : Data -> E.Encoder
encode data =
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
                    E.listIndef encode xs

        encodeInt : Int -> E.Encoder
        encodeInt i =
            -- NOTE: Technically, Plutus allows to encode integers up to uint64 and -uint63,
            -- but JavaScript (and thus Elm) do not support such large numbers. So we resort
            -- much sooner to bytes which would still lead to a valid encoding albeit different
            -- than a Rust or Haskell implementation around those edges.
            if (i >= 0 && i <= 9007199254740991) || (i < 0 && i >= -9007199254740992) then
                E.int i

            else if i >= 0 then
                E.tagged Tag.PositiveBigNum E.bytes (intToBytes i)

            else
                E.tagged Tag.NegativeBigNum E.bytes (intToBytes i)

        intToBytes : Int -> Bytes
        intToBytes n0 =
            let
                go n =
                    if n == 0 then
                        []

                    else
                        (n |> modBy 256 |> BE.unsignedInt8) :: go (n |> shiftRightBy 8)
            in
            BE.encode <|
                if n0 == 0 then
                    BE.unsignedInt8 0

                else
                    go n0 |> List.reverse |> BE.sequence
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
            E.associativeList encode encode xs

        List xs ->
            encodeList xs

        Int i ->
            encodeInt i

        HexBytes hexStr ->
            let
                bytes =
                    unhex hexStr
            in
            if Bytes.width bytes <= 64 then
                E.bytes bytes

            else
                E.sequence <|
                    E.beginBytes
                        :: List.foldr
                            (\chunk rest -> E.bytes chunk :: rest)
                            [ E.break ]
                            (Bytes.chunksOf 64 bytes)


unhex : String -> Bytes
unhex hexStr =
    Maybe.withDefault absurd (Hex.toBytes hexStr)


absurd : Bytes
absurd =
    E.encode (E.sequence [])
