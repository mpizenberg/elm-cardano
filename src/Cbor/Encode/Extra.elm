module Cbor.Encode.Extra exposing
    ( natural, integer
    , nonEmptyField
    , associativeList, indefiniteList, beginBytes
    )

{-| Extra CBOR encoding utility functions.

@docs natural, integer
@docs nonEmptyField
@docs associativeList, indefiniteList, beginBytes

-}

import Bytes.Comparable as Bytes
import Cbor
import Cbor.Encode as E
import Cbor.Tag as Tag
import Dict.Any
import Integer as I exposing (Integer)
import Natural as N exposing (Natural)


{-| Encode a natural number.
-}
natural : Natural -> E.Encoder
natural n =
    if isSafeNat n then
        E.int (N.toInt n)

    else if isU64 n then
        let
            msbLsb =
                N.divModBy (N.fromSafeInt <| 2 ^ 32) n
                    |> Maybe.map (\( msb, lsb ) -> ( N.toInt msb, N.toInt lsb ))
                    |> Maybe.withDefault ( 0, 0 )
        in
        E.any (Cbor.CborInt64 msbLsb)

    else
        let
            -- simple implementation with hex encoding
            -- TODO: improve this with a better performing approach if needed
            nAsBytes =
                N.toHexString n
                    |> prependWith0IfOddLength
                    |> Bytes.fromHexUnchecked
                    |> Bytes.toBytes
        in
        E.tagged Tag.PositiveBigNum E.bytes nAsBytes


{-| Encode a large integer number.
-}
integer : Integer -> E.Encoder
integer n =
    if I.isNonNegative n then
        natural (I.toNatural n)

    else if isSafeInt n then
        E.int (I.toInt n)

    else if isNegativeCborU64 n then
        let
            msbLsb =
                I.toNatural n
                    |> N.divModBy (N.fromSafeInt <| 2 ^ 32)
                    |> Maybe.map (\( msb, lsb ) -> ( -(N.toInt msb), N.toInt lsb ))
                    |> Maybe.withDefault ( 0, 0 )
        in
        E.any (Cbor.CborInt64 msbLsb)

    else
        -- Negative big number
        let
            -- simple implementation with hex encoding
            -- TODO: improve this with a better performing approach if needed
            nAsBytes =
                I.toHexString (I.add n I.one)
                    |> String.dropLeft 1
                    |> prependWith0IfOddLength
                    |> Bytes.fromHexUnchecked
                    |> Bytes.toBytes
        in
        E.tagged Tag.NegativeBigNum E.bytes nAsBytes


isSafeInt : Integer -> Bool
isSafeInt n =
    (n |> I.isLessThanOrEqual (I.fromSafeInt I.maxSafeInt))
        && (n |> I.isGreaterThan (I.fromSafeInt I.minSafeInt))


isSafeNat : Natural -> Bool
isSafeNat n =
    n |> N.isLessThanOrEqual (N.fromSafeInt N.maxSafeInt)


isU64 : Natural -> Bool
isU64 n =
    n |> N.isLessThan limit64Bits


{-| Check if n is >= -(2^64)

BEWARE the >= here and not > since negative CBOR numbers can go up to that.
Also we don’t check the number sign here, it’s the caller responsability.

-}
isNegativeCborU64 : Integer -> Bool
isNegativeCborU64 n =
    I.toNatural n |> N.isLessThanOrEqual limit64Bits


limit32Bits : Natural
limit32Bits =
    N.fromSafeInt (2 ^ 32)


limit64Bits : Natural
limit64Bits =
    N.mul limit32Bits limit32Bits


prependWith0IfOddLength : String -> String
prependWith0IfOddLength str =
    if modBy 2 (String.length str) == 0 then
        str

    else
        "0" ++ str


{-| Encode a foldable only if non empty.
-}
nonEmptyField :
    k
    -> (field -> Bool)
    -> (field -> E.Encoder)
    -> (record -> field)
    -> E.Step k record
    -> E.Step k record
nonEmptyField key isEmpty encode extract =
    E.optionalField key encode <|
        extract
            >> (\xs ->
                    if isEmpty xs then
                        Nothing

                    else
                        Just xs
               )


{-| Encode associative list with canonical ordering of the keys.

The keys in every map must be sorted lowest value to highest.
Sorting is performed on the bytes of the representation of the key
data items without paying attention to the 3/5 bit splitting for
major types. (Note that this rule allows maps that have keys of
different types, even though that is probably a bad practice that
could lead to errors in some canonicalization implementations.)
The sorting rules are:

  - If two keys have different lengths, the shorter one sorts
    earlier;

  - If two keys have the same length, the one with the lower value
    in (byte-wise) lexical order sorts earlier.

-}
associativeList : (k -> E.Encoder) -> (v -> E.Encoder) -> List ( k, v ) -> E.Encoder
associativeList encodeKey encodeValue pairs =
    Dict.Any.fromList (toCanonicalKey encodeKey) pairs
        |> Dict.Any.toList
        |> E.associativeList encodeKey encodeValue


toCanonicalKey : (k -> E.Encoder) -> k -> ( Int, String )
toCanonicalKey encodeKey k =
    let
        encodedKey =
            E.encode (encodeKey k)
                |> Bytes.fromBytes
                |> Bytes.toHex
    in
    ( String.length encodedKey, encodedKey )


{-| If you really need indefinite lists.
-}
indefiniteList : (a -> E.Encoder) -> List a -> E.Encoder
indefiniteList =
    E.indefiniteList


{-| If you really need to build bytes with indefinite arrays.
-}
beginBytes : E.Encoder
beginBytes =
    E.beginBytes
