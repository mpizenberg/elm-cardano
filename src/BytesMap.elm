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


type BytesMap a
    = BytesMap (Dict String a)



-------- Build


{-| Create an empty `BytesMap`.
-}
empty : BytesMap a
empty =
    BytesMap <|
        Dict.empty


{-| Create a `BytesMap` with one key-value pair.
-}
singleton : Bytes -> a -> BytesMap a
singleton k v =
    BytesMap <| Dict.singleton (hex k) v


{-| Insert a key-value pair into a `BytesMap`. Replaces value when there is a collision.
-}
insert : Bytes -> a -> BytesMap a -> BytesMap a
insert k v (BytesMap m) =
    BytesMap <| Dict.insert (hex k) v m


{-| Update the value of a `BytesMap` for a specific key with a given function.
-}
update : Bytes -> (Maybe a -> Maybe a) -> BytesMap a -> BytesMap a
update k f (BytesMap m) =
    BytesMap <| Dict.update (hex k) f m


{-| Remove a key-value pair from a `BytesMap`. If the key is not found, no changes
are made.
-}
remove : Bytes -> BytesMap a -> BytesMap a
remove k (BytesMap m) =
    BytesMap <| Dict.remove (hex k) m



-------- Query


{-| Determine if a `BytesMap` is empty.
-}
isEmpty : BytesMap a -> Bool
isEmpty (BytesMap m) =
    Dict.isEmpty m


{-| Determine if a key is in a `BytesMap`.
-}
member : Bytes -> BytesMap a -> Bool
member k (BytesMap m) =
    Dict.member (hex k) m


{-| Get the value associated with a key. If the key is not found, return `Nothing`. This is useful when you are not sure if a key will be in the `BytesMap`
-}
get : Bytes -> BytesMap a -> Maybe a
get k (BytesMap m) =
    Dict.get (hex k) m


{-| Determine the number of key-value pairs in the `BytesMap`.
-}
size : BytesMap a -> Int
size (BytesMap m) =
    Dict.size m



-------- Lists


{-| Get all of the keys in a `BytesMap`, sorted from lowest to highest.
-}
keys : BytesMap a -> List Bytes
keys (BytesMap m) =
    Dict.foldr (\k _ ks -> unhex k :: ks) [] m


{-| Get all of the values in a dictionary, in the order of their keys.
-}
values : BytesMap a -> List a
values (BytesMap m) =
    Dict.values m


{-| Convert a `BytesMap` into an association list of key-value pairs, sorted by keys.
-}
toList : BytesMap a -> List ( Bytes, a )
toList (BytesMap m) =
    Dict.foldr (\k v ks -> ( unhex k, v ) :: ks) [] m


{-| Convert an association list into a `BytesMap`.
-}
fromList : List ( Bytes, a ) -> BytesMap a
fromList =
    List.foldr (\( k, v ) -> insert k v) empty



-------- Transform


{-| Apply a function to all values in a `BytesMap`.
-}
map : (a -> b) -> BytesMap a -> BytesMap b
map f (BytesMap m) =
    BytesMap <| Dict.map (always f) m


{-| Apply a function to all keys and values in a `BytesMap`.
-}
mapWithKeys : (Bytes -> a -> b) -> BytesMap a -> BytesMap b
mapWithKeys f (BytesMap m) =
    BytesMap <| Dict.map (unhex >> f) m


{-| Fold over the values in a `BytesMap` from lowest key to highest key.
-}
foldl : (a -> b -> b) -> b -> BytesMap a -> b
foldl f zero (BytesMap m) =
    Dict.foldl (always f) zero m


{-| Fold over the key-value pairs in a `BytesMap` from lowest key to highest key.
-}
foldlWithKeys : (Bytes -> a -> b -> b) -> b -> BytesMap a -> b
foldlWithKeys f zero (BytesMap m) =
    Dict.foldl (unhex >> f) zero m


{-| Fold over the values in a `BytesMap` from highest key to lowest key.
-}
foldr : (a -> b -> b) -> b -> BytesMap a -> b
foldr f zero (BytesMap m) =
    Dict.foldr (always f) zero m


{-| Fold over the key-value pairs in a `BytesMap` from highest key to lowest key.
-}
foldrWithKeys : (Bytes -> a -> b -> b) -> b -> BytesMap a -> b
foldrWithKeys f zero (BytesMap m) =
    Dict.foldr (unhex >> f) zero m


{-| Keep only the values that pass the given test.
-}
filter : (a -> Bool) -> BytesMap a -> BytesMap a
filter f (BytesMap m) =
    BytesMap <| Dict.filter (always f) m


{-| Keep only the key-value pairs that pass the given test.
-}
filterWithKeys : (Bytes -> a -> Bool) -> BytesMap a -> BytesMap a
filterWithKeys f (BytesMap m) =
    BytesMap <| Dict.filter (unhex >> f) m



-------- Combine


{-| Combine two `BytesMap`. If there is a collision, preference is given to
the first `BytesMap`.
-}
union : BytesMap a -> BytesMap a -> BytesMap a
union (BytesMap left) (BytesMap right) =
    BytesMap <| Dict.union left right


{-| Keep a key-value pair when its key appears in the second `BytesMap`.
Preference is given to values in the first `BytesMap`.
-}
intersect : BytesMap a -> BytesMap a -> BytesMap a
intersect (BytesMap left) (BytesMap right) =
    BytesMap <| Dict.intersect left right


{-| Keep a key-value pair when its key does not appear in the second `BytesMap`.
-}
diff : BytesMap a -> BytesMap b -> BytesMap a
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
    -> BytesMap a
    -> BytesMap b
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
