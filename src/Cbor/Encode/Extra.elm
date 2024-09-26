module Cbor.Encode.Extra exposing
    ( natural, integer
    , nonEmptyField
    , associativeList
    )

{-| Extra CBOR encoding utility functions.

@docs natural, integer
@docs nonEmptyField
@docs associativeList

-}

import Bytes.Comparable as Bytes
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

    else
        -- TODO: if < 2^64 we should encode as u64 instead!
        let
            -- simple implementation with hex encoding
            -- TODO: improve this with a better performing approach if needed
            nAsBytes =
                N.toHexString n
                    |> prependWith0IfOddLength
                    |> Bytes.fromStringUnchecked
                    |> Bytes.toBytes
        in
        E.tagged Tag.PositiveBigNum E.bytes nAsBytes


{-| Encode a large integer number.
-}
integer : Integer -> E.Encoder
integer n =
    if isSafeInt n then
        E.int (I.toInt n)

    else if I.isNonNegative n then
        -- Positive n
        -- TODO: if 64-bit, we should encode as u64 instead!
        let
            -- simple implementation with hex encoding
            -- TODO: improve this with a better performing approach if needed
            nAsBytes =
                I.toHexString n
                    |> prependWith0IfOddLength
                    |> Bytes.fromStringUnchecked
                    |> Bytes.toBytes
        in
        E.tagged Tag.PositiveBigNum E.bytes nAsBytes

    else
        -- Negative n
        -- TODO: if 64-bit, we should encode as u64 instead!
        let
            -- simple implementation with hex encoding
            -- TODO: improve this with a better performing approach if needed
            nAsBytes =
                I.toHexString (I.add n I.one)
                    |> String.dropLeft 1
                    |> prependWith0IfOddLength
                    |> Bytes.fromStringUnchecked
                    |> Bytes.toBytes
        in
        E.tagged Tag.NegativeBigNum E.bytes nAsBytes


isSafeInt : Integer -> Bool
isSafeInt n =
    (n |> I.isLessThanOrEqual (I.fromSafeInt I.maxSafeInt))
        && (n |> I.isGreaterThanOrEqual (I.fromSafeInt I.minSafeInt))


isSafeNat : Natural -> Bool
isSafeNat n =
    n |> N.isLessThanOrEqual (N.fromSafeInt N.maxSafeInt)


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
                |> Bytes.toString
    in
    ( String.length encodedKey, encodedKey )
