module Bytes.Map exposing
    ( BytesMap
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, mapWithKeys, foldl, foldlWithKeys, foldr, foldrWithKeys, filter, filterWithKeys
    , union, intersect, diff, merge
    , toCbor, fromCbor
    )

{-| A `BytesMap` is a dictionnary mapping unique keys to values, where all keys are
byte strings.

Insert, remove, and query operations all take O(log n) time.


## BytesMaps

@docs BytesMap


## Build

@docs empty, singleton, insert, update, remove


## Query

@docs isEmpty, member, get, size


## Lists

@docs keys, values, toList, fromList


## Transform

@docs map, mapWithKeys, foldl, foldlWithKeys, foldr, foldrWithKeys, filter, filterWithKeys


## Combine

@docs union, intersect, diff, merge


## Encode / Decode

@docs toCbor, fromCbor

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Dict exposing (Dict)



-- TODO: Because we rely on the sorting of the hex string,
-- it is super important to check that we only use
-- lower case OR upper case letters, but no mix
-------- BytesMaps


{-| Dictionary mapping [Bytes] keys to values.
-}
type BytesMap k v
    = BytesMap (Dict String v)



-------- Build


{-| Create an empty `BytesMap`.
-}
empty : BytesMap k v
empty =
    BytesMap <|
        Dict.empty


{-| Create a `BytesMap` with one key-value pair.
-}
singleton : Bytes k -> v -> BytesMap k v
singleton k v =
    BytesMap <| Dict.singleton (Bytes.toString k) v


{-| Insert a key-value pair into a `BytesMap`. Replaces value when there is a collision.
-}
insert : Bytes k -> v -> BytesMap k v -> BytesMap k v
insert k v (BytesMap m) =
    BytesMap <| Dict.insert (Bytes.toString k) v m


{-| Update the value of a `BytesMap` for a specific key with a given function.
-}
update : Bytes k -> (Maybe v -> Maybe v) -> BytesMap k v -> BytesMap k v
update k f (BytesMap m) =
    BytesMap <| Dict.update (Bytes.toString k) f m


{-| Remove a key-value pair from a `BytesMap`. If the key is not found, no changes
are made.
-}
remove : Bytes k -> BytesMap k v -> BytesMap k v
remove k (BytesMap m) =
    BytesMap <| Dict.remove (Bytes.toString k) m



-------- Query


{-| Determine if a `BytesMap` is empty.
-}
isEmpty : BytesMap k v -> Bool
isEmpty (BytesMap m) =
    Dict.isEmpty m


{-| Determine if a key is in a `BytesMap`.
-}
member : Bytes k -> BytesMap k v -> Bool
member k (BytesMap m) =
    Dict.member (Bytes.toString k) m


{-| Get the value associated with a key. If the key is not found, return `Nothing`. This is useful when you are not sure if a key will be in the `BytesMap`
-}
get : Bytes k -> BytesMap k v -> Maybe v
get k (BytesMap m) =
    Dict.get (Bytes.toString k) m


{-| Determine the number of key-value pairs in the `BytesMap`.
-}
size : BytesMap k v -> Int
size (BytesMap m) =
    Dict.size m



-------- Lists


{-| Get all of the keys in a `BytesMap`, sorted from lowest to highest.
-}
keys : BytesMap k v -> List (Bytes k)
keys (BytesMap m) =
    Dict.foldr (\k _ ks -> Bytes.fromStringUnchecked k :: ks) [] m


{-| Get all of the values in a dictionary, in the order of their keys.
-}
values : BytesMap k v -> List v
values (BytesMap m) =
    Dict.values m


{-| Convert a `BytesMap` into an association list of key-value pairs, sorted by keys.
-}
toList : BytesMap k v -> List ( Bytes k, v )
toList (BytesMap m) =
    Dict.foldr (\k v ks -> ( Bytes.fromStringUnchecked k, v ) :: ks) [] m


{-| Convert an association list into a `BytesMap`.
-}
fromList : List ( Bytes k, v ) -> BytesMap k v
fromList =
    List.foldr (\( k, v ) -> insert k v) empty



-------- Transform


{-| Apply a function to all values in a `BytesMap`.
-}
map : (a -> b) -> BytesMap k a -> BytesMap k b
map f (BytesMap m) =
    BytesMap <| Dict.map (always f) m


{-| Apply a function to all keys and values in a `BytesMap`.
-}
mapWithKeys : (Bytes k -> a -> b) -> BytesMap k a -> BytesMap k b
mapWithKeys f (BytesMap m) =
    BytesMap <| Dict.map (Bytes.fromStringUnchecked >> f) m


{-| Fold over the values in a `BytesMap` from lowest key to highest key.
-}
foldl : (v -> result -> result) -> result -> BytesMap k v -> result
foldl f zero (BytesMap m) =
    Dict.foldl (always f) zero m


{-| Fold over the key-value pairs in a `BytesMap` from lowest key to highest key.
-}
foldlWithKeys : (Bytes k -> v -> result -> result) -> result -> BytesMap k v -> result
foldlWithKeys f zero (BytesMap m) =
    Dict.foldl (Bytes.fromStringUnchecked >> f) zero m


{-| Fold over the values in a `BytesMap` from highest key to lowest key.
-}
foldr : (v -> result -> result) -> result -> BytesMap k v -> result
foldr f zero (BytesMap m) =
    Dict.foldr (always f) zero m


{-| Fold over the key-value pairs in a `BytesMap` from highest key to lowest key.
-}
foldrWithKeys : (Bytes k -> v -> result -> result) -> result -> BytesMap k v -> result
foldrWithKeys f zero (BytesMap m) =
    Dict.foldr (Bytes.fromStringUnchecked >> f) zero m


{-| Keep only the values that pass the given test.
-}
filter : (v -> Bool) -> BytesMap k v -> BytesMap k v
filter f (BytesMap m) =
    BytesMap <| Dict.filter (always f) m


{-| Keep only the key-value pairs that pass the given test.
-}
filterWithKeys : (Bytes k -> v -> Bool) -> BytesMap k v -> BytesMap k v
filterWithKeys f (BytesMap m) =
    BytesMap <| Dict.filter (Bytes.fromStringUnchecked >> f) m



-------- Combine


{-| Combine two `BytesMap`. If there is a collision, preference is given to
the first `BytesMap`.
-}
union : BytesMap k v -> BytesMap k v -> BytesMap k v
union (BytesMap left) (BytesMap right) =
    BytesMap <| Dict.union left right


{-| Keep a key-value pair when its key appears in the second `BytesMap`.
Preference is given to values in the first `BytesMap`.
-}
intersect : BytesMap k v -> BytesMap k v -> BytesMap k v
intersect (BytesMap left) (BytesMap right) =
    BytesMap <| Dict.intersect left right


{-| Keep a key-value pair when its key does not appear in the second `BytesMap`.
-}
diff : BytesMap k v -> BytesMap k v -> BytesMap k v
diff (BytesMap left) (BytesMap right) =
    BytesMap <| Dict.diff left right


{-| The most general way of combining two `BytesMap`. You provide three accumulators for when a given key appears:

  - Only in the left `BytesMap`.
  - In both `BytesMap`.
  - Only in the right `BytesMap`.

You then traverse all the keys from lowest to highest, building up whatever you want.

-}
merge :
    (Bytes k -> a -> result -> result)
    -> (Bytes k -> a -> b -> result -> result)
    -> (Bytes k -> b -> result -> result)
    -> BytesMap k a
    -> BytesMap k b
    -> result
    -> result
merge whenLeft whenBoth whenRight (BytesMap left) (BytesMap right) =
    Dict.merge
        (Bytes.fromStringUnchecked >> whenLeft)
        (Bytes.fromStringUnchecked >> whenBoth)
        (Bytes.fromStringUnchecked >> whenRight)
        left
        right


{-| Cbor encoder.
-}
toCbor : (v -> E.Encoder) -> BytesMap k v -> E.Encoder
toCbor valueEncoder (BytesMap data) =
    let
        keyEncoder =
            Bytes.fromStringUnchecked >> Bytes.toCbor
    in
    EE.ledgerDict keyEncoder valueEncoder data


{-| CBOR decoder.
-}
fromCbor : D.Decoder v -> D.Decoder (BytesMap k v)
fromCbor valueDecoder =
    D.map BytesMap <|
        D.dict
            -- Convert the key from Bytes to hex String
            (D.map (Bytes.toString << Bytes.fromBytes) D.bytes)
            valueDecoder
