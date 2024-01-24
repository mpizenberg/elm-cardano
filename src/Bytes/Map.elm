module Bytes.Map exposing
    ( BytesMap
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, mapWithKeys, foldl, foldlWithKeys, foldr, foldrWithKeys, filter, filterWithKeys
    , union, intersect, diff, merge
    , toCbor
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


## Encode

@docs toCbor

-}

import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Dict exposing (Dict)



-------- BytesMaps


{-| Dictionary mapping [Bytes] keys to values.
-}
type BytesMap k v
    = BytesMap
        { k2bytes : k -> Bytes Any
        , kvs : Dict String v
        }



-------- Build


{-| Create an empty `BytesMap`.
-}
empty : (k -> Bytes Any) -> BytesMap k v
empty k2bytes =
    BytesMap
        { k2bytes = k2bytes
        , kvs = Dict.empty
        }


{-| Create a `BytesMap` with one key-value pair.
-}
singleton : (k -> Bytes Any) -> k -> v -> BytesMap k v
singleton k2bytes k v =
    BytesMap
        { k2bytes = k2bytes
        , kvs = Dict.singleton (k2str k2bytes k) v
        }


{-| Insert a key-value pair into a `BytesMap`. Replaces value when there is a collision.
-}
insert : k -> v -> BytesMap k v -> BytesMap k v
insert k v (BytesMap props) =
    BytesMap
        { props
            | kvs = Dict.insert (k2str props.k2bytes k) v props.kvs
        }


{-| Update the value of a `BytesMap` for a specific key with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> BytesMap k v -> BytesMap k v
update k f (BytesMap props) =
    BytesMap
        { props
            | kvs = Dict.update (k2str props.k2bytes k) f props.kvs
        }


{-| Remove a key-value pair from a `BytesMap`. If the key is not found, no changes
are made.
-}
remove : k -> BytesMap k v -> BytesMap k v
remove k (BytesMap props) =
    BytesMap
        { props
            | kvs = Dict.remove (k2str props.k2bytes k) props.kvs
        }



-------- Query


{-| Determine if a `BytesMap` is empty.
-}
isEmpty : BytesMap k v -> Bool
isEmpty (BytesMap props) =
    Dict.isEmpty props.kvs


{-| Determine if a key is in a `BytesMap`.
-}
member : k -> BytesMap k v -> Bool
member k (BytesMap props) =
    Dict.member (k2str props.k2bytes k) props.kvs


{-| Get the value associated with a key. If the key is not found, return `Nothing`. This is useful when you are not sure if a key will be in the `BytesMap`
-}
get : k -> BytesMap k v -> Maybe v
get k (BytesMap props) =
    Dict.get (k2str props.k2bytes k) props.kvs


{-| Determine the number of key-value pairs in the `BytesMap`.
-}
size : BytesMap k v -> Int
size (BytesMap props) =
    Dict.size props.kvs



-------- Lists


{-| Get all of the keys in a `BytesMap`, sorted from lowest to highest.
-}
keys : BytesMap k v -> List (Bytes Any)
keys (BytesMap props) =
    Dict.foldr (\k _ ks -> Bytes.fromStringUnchecked k :: ks) [] props.kvs


{-| Get all of the values in a dictionary, in the order of their keys.
-}
values : BytesMap k v -> List v
values (BytesMap props) =
    Dict.values props.kvs


{-| Convert a `BytesMap` into an association list of key-value pairs, sorted by keys.
-}
toList : BytesMap k v -> List ( Bytes Any, v )
toList (BytesMap props) =
    Dict.foldr (\k v ks -> ( Bytes.fromStringUnchecked k, v ) :: ks) [] props.kvs


{-| Convert an association list into a `BytesMap`.
-}
fromList : (k -> Bytes Any) -> List ( k, v ) -> BytesMap k v
fromList k2bytes =
    List.foldr (\( k, v ) -> insert k v) (empty k2bytes)



-------- Transform


{-| Apply a function to all values in a `BytesMap`.
-}
map : (a -> b) -> BytesMap k a -> BytesMap k b
map f (BytesMap props) =
    BytesMap
        { k2bytes = props.k2bytes
        , kvs = Dict.map (always f) props.kvs
        }


{-| Apply a function to all keys and values in a `BytesMap`.
-}
mapWithKeys : (Bytes Any -> k) -> (k -> a -> b) -> BytesMap k a -> BytesMap k b
mapWithKeys bytes2k f (BytesMap props) =
    BytesMap
        { k2bytes = props.k2bytes
        , kvs = Dict.map (Bytes.fromStringUnchecked >> bytes2k >> f) props.kvs
        }


{-| Fold over the values in a `BytesMap` from lowest key to highest key.
-}
foldl : (v -> result -> result) -> result -> BytesMap k v -> result
foldl f zero (BytesMap props) =
    Dict.foldl (always f) zero props.kvs


{-| Fold over the key-value pairs in a `BytesMap` from lowest key to highest key.
-}
foldlWithKeys : (Bytes Any -> k) -> (k -> v -> result -> result) -> result -> BytesMap k v -> result
foldlWithKeys bytes2k f zero (BytesMap props) =
    Dict.foldl (Bytes.fromStringUnchecked >> bytes2k >> f) zero props.kvs


{-| Fold over the values in a `BytesMap` from highest key to lowest key.
-}
foldr : (v -> result -> result) -> result -> BytesMap k v -> result
foldr f zero (BytesMap props) =
    Dict.foldr (always f) zero props.kvs


{-| Fold over the key-value pairs in a `BytesMap` from highest key to lowest key.
-}
foldrWithKeys : (Bytes Any -> k) -> (k -> v -> result -> result) -> result -> BytesMap k v -> result
foldrWithKeys bytes2k f zero (BytesMap props) =
    Dict.foldr (Bytes.fromStringUnchecked >> bytes2k >> f) zero props.kvs


{-| Keep only the values that pass the given test.
-}
filter : (v -> Bool) -> BytesMap k v -> BytesMap k v
filter f (BytesMap props) =
    BytesMap
        { props
            | kvs = Dict.filter (always f) props.kvs
        }


{-| Keep only the key-value pairs that pass the given test.
-}
filterWithKeys : (Bytes Any -> k) -> (k -> v -> Bool) -> BytesMap k v -> BytesMap k v
filterWithKeys bytes2k f (BytesMap props) =
    BytesMap
        { props
            | kvs = Dict.filter (Bytes.fromStringUnchecked >> bytes2k >> f) props.kvs
        }



-------- Combine


{-| Combine two `BytesMap`. If there is a collision, preference is given to
the first `BytesMap`.
-}
union : BytesMap k v -> BytesMap k v -> BytesMap k v
union (BytesMap lProps) (BytesMap rProps) =
    BytesMap
        { lProps
            | kvs = Dict.union lProps.kvs rProps.kvs
        }


{-| Keep a key-value pair when its key appears in the second `BytesMap`.
Preference is given to values in the first `BytesMap`.
-}
intersect : BytesMap k v -> BytesMap k v -> BytesMap k v
intersect (BytesMap lProps) (BytesMap rProps) =
    BytesMap
        { lProps
            | kvs = Dict.intersect lProps.kvs rProps.kvs
        }


{-| Keep a key-value pair when its key does not appear in the second `BytesMap`.
-}
diff : BytesMap k v -> BytesMap k v -> BytesMap k v
diff (BytesMap lProps) (BytesMap rProps) =
    BytesMap
        { lProps
            | kvs = Dict.diff lProps.kvs rProps.kvs
        }


{-| The most general way of combining two `BytesMap`. You provide three accumulators for when a given key appears:

  - Only in the left `BytesMap`.
  - In both `BytesMap`.
  - Only in the right `BytesMap`.

You then traverse all the keys from lowest to highest, building up whatever you want.

-}
merge :
    (Bytes Any -> k)
    -> (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> BytesMap k a
    -> BytesMap k b
    -> result
    -> result
merge k2bytes whenLeft whenBoth whenRight (BytesMap lProps) (BytesMap rProps) =
    Dict.merge
        (Bytes.fromStringUnchecked >> k2bytes >> whenLeft)
        (Bytes.fromStringUnchecked >> k2bytes >> whenBoth)
        (Bytes.fromStringUnchecked >> k2bytes >> whenRight)
        lProps.kvs
        rProps.kvs


{-| Cbor encoder.
-}
toCbor : (v -> E.Encoder) -> BytesMap k v -> E.Encoder
toCbor valueEncoder (BytesMap data) =
    let
        keyEncoder =
            Bytes.fromStringUnchecked >> Bytes.toCbor
    in
    EE.ledgerDict keyEncoder valueEncoder data.kvs


k2str : (k -> Bytes Any) -> k -> String
k2str k2bytes =
    Bytes.toString << k2bytes
