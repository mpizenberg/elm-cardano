module Json.Decode.Extra exposing (longString)

{-| Extra JSON decoders.
-}

import Json.Decode as JE


{-| Decoder for strings or an array of strings.
-}
longString : String -> JE.Value
longString str =
    JD.oneOf
        [ JD.string
        , JD.list JD.string |> D.map String.concat
        ]
