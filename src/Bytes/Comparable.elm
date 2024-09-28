module Bytes.Comparable exposing
    ( Bytes
    , Any, toAny
    , chunksOf, width, isEmpty
    , fromBytes, fromHex, fromHexUnchecked, fromText, fromU8
    , toBytes, toHex, toText, toCbor, toU8
    )

{-| Comparable Bytes

-- TODO: Because we rely on the sorting of the hex string,
-- it is super important to check that we only use
-- lower case letters, but no uppercase.

@docs Bytes
@docs Any, toAny
@docs chunksOf, width, isEmpty
@docs fromBytes, fromHex, fromHexUnchecked, fromText, fromU8
@docs toBytes, toHex, toText, toCbor, toU8

-}

import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Cbor.Encode as Cbor
import Hex.Convert as Hex


{-| A custom `Bytes` type that is comparable with `==`.

Useful as otherwise, the original `Bytes` type from `elm/bytes` package cannot be used to compare for equality with `==`.
The phantom type parameter `a` indicates what type of Bytes are stored.

-}
type Bytes a
    = Bytes String


{-| A catch-all phantom type for bytes.
-}
type Any
    = Any Never


{-| Convert any type of bytes to `Bytes Any`.
-}
toAny : Bytes a -> Bytes Any
toAny (Bytes str) =
    Bytes str


{-| Create a [Bytes] object from individual U8 integers.
-}
fromU8 : List Int -> Bytes a
fromU8 =
    List.map E.unsignedInt8 >> E.sequence >> E.encode >> fromBytes


{-| Check if this is empy.
-}
isEmpty : Bytes a -> Bool
isEmpty (Bytes str) =
    String.isEmpty str


{-| Length in bytes.
-}
width : Bytes a -> Int
width (Bytes str) =
    String.length str // 2


{-| Create a [Bytes] object from a hex-encoded string.
-}
fromHex : String -> Maybe (Bytes a)
fromHex str =
    str |> Hex.toBytes |> Maybe.map (always <| Bytes (String.toLower str))


{-| Same as [fromHex] except it does not check that the hex-encoded string is well formed.
It is your responsability.
-}
fromHexUnchecked : String -> Bytes a
fromHexUnchecked =
    Bytes


{-| Create a [Bytes] with some text encoded as UTF8.
-}
fromText : String -> Bytes a
fromText str =
    E.encode (E.string str) |> fromBytes


{-| Create a [Bytes] object from an elm/bytes [Bytes.Bytes].
-}
fromBytes : Bytes.Bytes -> Bytes a
fromBytes bs =
    Bytes (String.toLower <| Hex.toString bs)


{-| Convert [Bytes] into a hex-encoded String.
-}
toHex : Bytes a -> String
toHex (Bytes str) =
    str


{-| Convert [Bytes] into a UTF8 String.
-}
toText : Bytes a -> Maybe String
toText bs =
    D.decode (D.string <| width bs) (toBytes bs)


{-| Convert [Bytes] into elm/bytes [Bytes.Bytes].
-}
toBytes : Bytes a -> Bytes.Bytes
toBytes (Bytes str) =
    str |> Hex.toBytes |> Maybe.withDefault absurd


{-| Cbor encoder.
-}
toCbor : Bytes a -> Cbor.Encoder
toCbor =
    toBytes >> Cbor.bytes


absurd : Bytes.Bytes
absurd =
    E.encode (E.sequence [])


{-| Break a Bytestring into a list of chunks. Chunks are of the given width,
except the last chunk which is only _at most_ the given width.
-}
chunksOf : Int -> Bytes a -> List (Bytes a)
chunksOf n =
    toBytes
        >> (\bs ->
                D.decode
                    (D.loop ( Bytes.width bs, [] ) <|
                        \( w, chunks ) ->
                            if w == 0 then
                                D.succeed (D.Done <| List.reverse chunks)

                            else
                                let
                                    len =
                                        min w n
                                in
                                D.bytes len
                                    |> D.map (\chunk -> D.Loop ( w - len, chunk :: chunks ))
                    )
                    bs
                    |> Maybe.withDefault []
           )
        >> List.map fromBytes


{-| Convert a given [Bytes] into a list of U8 integers.
-}
toU8 : Bytes a -> List Int
toU8 bs =
    bytesToU8 (width bs) (toBytes bs)


bytesToU8 : Int -> Bytes.Bytes -> List Int
bytesToU8 size bs =
    D.decode (D.loop ( size, [] ) splitStep) bs
        |> Maybe.withDefault []


splitStep : ( Int, List Int ) -> D.Decoder (D.Step ( Int, List Int ) (List Int))
splitStep ( size, u8s ) =
    if size <= 0 then
        D.succeed (D.Done <| List.reverse u8s)

    else
        D.map (\u8 -> D.Loop ( size - 1, u8 :: u8s )) D.unsignedInt8
