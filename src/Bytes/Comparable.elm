module Bytes.Comparable exposing
    ( Bytes
    , chunksOf, width
    , bytes, fromBytes, fromString, fromStringUnchecked, fromDecimal
    , toBytes, toString, toCbor, toDecimal, toWord8s
    )

{-| Comparable Bytes

@docs Bytes
@docs chunksOf, width
@docs bytes, fromBytes, fromString, fromStringUnchecked, fromDecimal
@docs toBytes, toString, toCbor, toDecimal, toWord8s

-}

import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Cbor.Encode as Cbor
import Hex as HexString
import Hex.Convert as Hex


{-| A custom `Bytes` type that is comparable with `==`.

Useful as otherwise, the original `Bytes` type from `elm/bytes` package cannot be used to compare for equality with `==`.

-}
type Bytes
    = Bytes String


{-| Create a [Bytes] object from individual U8 integers.
-}
bytes : List Int -> Bytes
bytes =
    List.map E.unsignedInt8 >> E.sequence >> E.encode >> fromBytes


{-| Length in bytes.
-}
width : Bytes -> Int
width (Bytes str) =
    String.length str // 2


{-| Create a [Bytes] object from a hex-encoded string.
-}
fromString : String -> Maybe Bytes
fromString str =
    str |> Hex.toBytes |> Maybe.map (always <| Bytes str)


{-| Same as [fromString] except it does not check that the hex-encoded string is well formed.
It is your responsability.
-}
fromStringUnchecked : String -> Bytes
fromStringUnchecked =
    Bytes


{-| Create a [Bytes] object from an elm/bytes [Bytes.Bytes].
-}
fromBytes : Bytes.Bytes -> Bytes
fromBytes bs =
    Bytes (Hex.toString bs)


{-| Convert a decimal integer to [Bytes].
-}
fromDecimal : Int -> Bytes
fromDecimal d =
    let
        str =
            HexString.toString d
    in
    if modBy 2 (String.length str) == 0 then
        Bytes str

    else
        Bytes <| "0" ++ str


{-| Convert [Bytes] into a hex-encoded String.
-}
toString : Bytes -> String
toString (Bytes str) =
    str


{-| Convert [Bytes] into elm/bytes [Bytes.Bytes].
-}
toBytes : Bytes -> Bytes.Bytes
toBytes (Bytes str) =
    str |> Hex.toBytes |> Maybe.withDefault absurd


{-| Convert a [Bytes] into its decimal equivalent.
-}
toDecimal : Bytes -> Int
toDecimal (Bytes str) =
    case HexString.fromString (String.toLower str) of
        Ok d ->
            d

        Err _ ->
            0


{-| Cbor encoder.
-}
toCbor : Bytes -> Cbor.Encoder
toCbor =
    toBytes >> Cbor.bytes


absurd : Bytes.Bytes
absurd =
    E.encode (E.sequence [])


{-| Break a Bytestring into a list of chunks. Chunks are of the given width,
except the last chunk which is only _at most_ the given width.
-}
chunksOf : Int -> Bytes -> List Bytes
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


{-| Convert a given [Bytes] into a list of decimal byte values.
-}
toWord8s : Bytes -> List Int
toWord8s =
    chunksOf 1 >> List.map toDecimal
