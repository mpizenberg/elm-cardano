module ElmCardano.KeyValuePair exposing (..)


type KeyValuePair k v
    = Def (List ( k, v ))
    | Indef (List ( k, v ))


isEmpty : KeyValuePair k v -> Bool
isEmpty kv =
    case kv of
        Def [] ->
            True

        Indef [] ->
            True

        _ ->
            False
