module Cbor.Encode.Extra exposing
    ( natural
    , nonEmptyField
    , ledgerList, ledgerDict, ledgerAssociativeList
    )

{-| Extra CBOR encoding utility functions.

@docs natural
@docs nonEmptyField
@docs ledgerList, ledgerDict, ledgerAssociativeList

-}

import Bytes.Comparable as Bytes
import Cbor.Encode as E
import Cbor.Tag as Tag
import Dict exposing (Dict)
import Natural as N exposing (Natural)


{-| Encode a natural number.
-}
natural : Natural -> E.Encoder
natural n =
    if isSafeInt n then
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


isSafeInt : Natural -> Bool
isSafeInt n =
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


{-| List CBOR encoder that encodes values as indefinite sequences
if containing 24 or more elements, and as finite for 23 or less elements.
-}
ledgerList : (v -> E.Encoder) -> List v -> E.Encoder
ledgerList valueEncoder list =
    if List.length list <= 23 then
        E.list valueEncoder list

    else
        E.indefiniteList valueEncoder list


{-| Dict CBOR encoder that encodes dicts as indefinite sequences
if the dict contains 24 or more elements, and as finite for 23 or less elements.
-}
ledgerDict : (k -> E.Encoder) -> (v -> E.Encoder) -> Dict k v -> E.Encoder
ledgerDict keyEncoder valueEncoder dict =
    if Dict.size dict <= 23 then
        E.dict keyEncoder valueEncoder dict

    else
        E.sequence <|
            E.beginDict
                :: Dict.foldl
                    (\key value acc -> E.keyValue keyEncoder valueEncoder ( key, value ) :: acc)
                    [ E.break ]
                    dict


{-| Associative list CBOR encoder that encodes (key,value) pairs as indefinite sequences
if containing 24 or more elements, and as finite for 23 or less elements.
-}
ledgerAssociativeList : (k -> E.Encoder) -> (v -> E.Encoder) -> List ( k, v ) -> E.Encoder
ledgerAssociativeList keyEncoder valueEncoder list =
    if List.length list <= 23 then
        E.associativeList keyEncoder valueEncoder list

    else
        E.sequence <|
            E.beginDict
                :: List.foldl
                    (\keyValuePair acc -> E.keyValue keyEncoder valueEncoder keyValuePair :: acc)
                    [ E.break ]
                    list
