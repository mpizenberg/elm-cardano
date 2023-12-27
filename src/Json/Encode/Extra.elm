module Json.Encode.Extra exposing (longString)

{-| Extra JSON encoding functions.
-}

import Json.Encode as JE
import String.Extra as String


{-| Encoder for strings longer than 64 bytes.

If the string is longer than 64 bytes, it'll be converted into an array of
strings where the last one is at most 64 bytes.

-}
longString : String -> JE.Value
longString str =
    if String.length str > 64 then
        JE.list JE.string (String.chunksOf 64 str)

    else
        JE.string str
