module Extra.List exposing (chunksOf, get, get64, indexedMap64, last, sublist, take64, updateAt)

import Blake2b.Int64 as Int64 exposing (Int64(..))


chunksOf : Int -> List a -> List (List a)
chunksOf size list =
    if List.isEmpty list then
        []

    else
        List.take size list :: chunksOf size (List.drop size list)


get : Int -> List a -> Maybe a
get n =
    List.drop n >> List.head


get64 : Int64 -> List a -> Maybe a
get64 (Int64 iMS iLS) list =
    if iMS > 0 then
        let
            chunked =
                chunksOf 0xFFFFFFFF list
        in
        get iMS chunked
            |> Maybe.andThen (get iLS)

    else
        get iLS list


take64 : Int64 -> List a -> List a
take64 (Int64 iMS iLS) list =
    if iMS > 0 then
        let
            chunked =
                chunksOf 0xFFFFFFFF list
        in
        List.take iMS chunked |> List.concat |> List.take iLS

    else
        List.take iLS list


indexedMap64 : (Int64 -> a -> b) -> List a -> List b
indexedMap64 fn list =
    List.foldl
        (\x ( acc, i ) ->
            ( acc ++ [ fn i x ], Int64.add i (Int64 0 1) )
        )
        ( [], Int64 0 0 )
        list
        |> Tuple.first


updateAt : Int -> (a -> a) -> List a -> List a
updateAt i fn =
    List.indexedMap
        (\j x ->
            if j == i then
                fn x

            else
                x
        )


sublist : Int -> Int -> List a -> List a
sublist start length =
    List.drop start >> List.take length


last : List a -> Maybe a
last list =
    case List.drop (List.length list - 1) list of
        [] ->
            Nothing

        x :: _ ->
            Just x
