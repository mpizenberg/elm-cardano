module Hex.Convert.Extra exposing (fromString)

import Bytes exposing (Bytes)
import Hex.Extra as Hex


fromString : String -> Bytes
fromString str =
    Hex.toBytes <| Hex.fromString str
