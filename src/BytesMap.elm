module BytesMap exposing
    ( BytesMap
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, mapWithKeys, foldl, foldlWithKeys, foldr, foldrWithKeys, filter, filterWithKeys
    , union, intersect, diff, merge
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

-}

import Bytes exposing (Bytes)
import Bytes.Encode as E
import Dict exposing (Dict)
import Hex.Convert as Hex



-------- BytesMaps


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
singleton : Bytes -> v -> BytesMap k v
singleton k v =
    BytesMap <| Dict.singleton (hex k) v


{-| Insert a key-value pair into a `BytesMap`. Replaces value when there is a collision.
-}
insert : Bytes -> v -> BytesMap k v -> BytesMap k v
insert k v (BytesMap m) =
    BytesMap <| Dict.insert (hex k) v m


{-| Update the value of a `BytesMap` for a specific key with a given function.
-}
update : Bytes -> (Maybe v -> Maybe v) -> BytesMap k v -> BytesMap k v
update k f (BytesMap m) =
    BytesMap <| Dict.update (hex k) f m


{-| Remove a key-value pair from a `BytesMap`. If the key is not found, no changes
are made.
-}
remove : Bytes -> BytesMap k v -> BytesMap k v
remove k (BytesMap m) =
    BytesMap <| Dict.remove (hex k) m



-------- Query


{-| Determine if a `BytesMap` is empty.
-}
isEmpty : BytesMap k v -> Bool
isEmpty (BytesMap m) =
    Dict.isEmpty m


{-| Determine if a key is in a `BytesMap`.
-}
member : Bytes -> BytesMap k v -> Bool
member k (BytesMap m) =
    Dict.member (hex k) m


{-| Get the value associated with a key. If the key is not found, return `Nothing`. This is useful when you are not sure if a key will be in the `BytesMap`
-}
get : Bytes -> BytesMap k v -> Maybe v
get k (BytesMap m) =
    Dict.get (hex k) m


{-| Determine the number of key-value pairs in the `BytesMap`.
-}
size : BytesMap k v -> Int
size (BytesMap m) =
    Dict.size m



-------- Lists


{-| Get all of the keys in a `BytesMap`, sorted from lowest to highest.
-}
keys : BytesMap k v -> List Bytes
keys (BytesMap m) =
    Dict.foldr (\k _ ks -> unhex k :: ks) [] m


{-| Get all of the values in a dictionary, in the order of their keys.
-}
values : BytesMap k v -> List v
values (BytesMap m) =
    Dict.values m


{-| Convert a `BytesMap` into an association list of key-value pairs, sorted by keys.
-}
toList : BytesMap k v -> List ( Bytes, v )
toList (BytesMap m) =
    Dict.foldr (\k v ks -> ( unhex k, v ) :: ks) [] m


{-| Convert an association list into a `BytesMap`.
-}
fromList : List ( Bytes, v ) -> BytesMap k v
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
mapWithKeys : (Bytes -> a -> b) -> BytesMap k a -> BytesMap k b
mapWithKeys f (BytesMap m) =
    BytesMap <| Dict.map (unhex >> f) m


{-| Fold over the values in a `BytesMap` from lowest key to highest key.
-}
foldl : (v -> result -> result) -> result -> BytesMap k v -> result
foldl f zero (BytesMap m) =
    Dict.foldl (always f) zero m


{-| Fold over the key-value pairs in a `BytesMap` from lowest key to highest key.
-}
foldlWithKeys : (Bytes -> v -> result -> result) -> result -> BytesMap k v -> result
foldlWithKeys f zero (BytesMap m) =
    Dict.foldl (unhex >> f) zero m


{-| Fold over the values in a `BytesMap` from highest key to lowest key.
-}
foldr : (v -> result -> result) -> result -> BytesMap k v -> result
foldr f zero (BytesMap m) =
    Dict.foldr (always f) zero m


{-| Fold over the key-value pairs in a `BytesMap` from highest key to lowest key.
-}
foldrWithKeys : (Bytes -> v -> result -> result) -> result -> BytesMap k v -> result
foldrWithKeys f zero (BytesMap m) =
    Dict.foldr (unhex >> f) zero m


{-| Keep only the values that pass the given test.
-}
filter : (v -> Bool) -> BytesMap k v -> BytesMap k v
filter f (BytesMap m) =
    BytesMap <| Dict.filter (always f) m


{-| Keep only the key-value pairs that pass the given test.
-}
filterWithKeys : (Bytes -> v -> Bool) -> BytesMap k v -> BytesMap k v
filterWithKeys f (BytesMap m) =
    BytesMap <| Dict.filter (unhex >> f) m



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
    (Bytes -> a -> result -> result)
    -> (Bytes -> a -> b -> result -> result)
    -> (Bytes -> b -> result -> result)
    -> BytesMap k a
    -> BytesMap k b
    -> result
    -> result
merge whenLeft whenBoth whenRight (BytesMap left) (BytesMap right) =
    Dict.merge
        (unhex >> whenLeft)
        (unhex >> whenBoth)
        (unhex >> whenRight)
        left
        right



-------- Internal


hex : Bytes -> String
hex =
    Hex.toString


unhex : String -> Bytes
unhex =
    let
        absurd =
            E.sequence [] |> E.encode
    in
    Hex.toBytes >> Maybe.withDefault absurd
