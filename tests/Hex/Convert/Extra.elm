module Hex.Convert.Extra exposing (..)

import Bytes exposing (Bytes)
import Hex.Convert as Hex


fromString : String -> Bytes
fromString str =
    case Hex.toBytes str of
        Nothing ->
            Debug.todo <| "Hex.Convert.Extra.fromString: not a valid hex string: " ++ str

        Just hex ->
            hex
