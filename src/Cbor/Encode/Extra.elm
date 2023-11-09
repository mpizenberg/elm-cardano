module Cbor.Encode.Extra exposing
    ( nonEmptyField
    , ledgerDict, ledgerAssociativeList
    )

{-| Extra CBOR encoding utility functions.

@docs nonEmptyField
@docs ledgerDict, ledgerAssociativeList

-}

import Cbor.Encode as E
import Dict exposing (Dict)


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
