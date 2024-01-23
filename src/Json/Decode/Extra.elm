module Json.Decode.Extra exposing (longString)

{-| Extra JSON decoders.
-}

import Json.Decode as JD


{-| Decoder for strings or an array of strings.
-}
longString : JD.Decoder String
longString =
    JD.oneOf
        [ JD.string
        , JD.list JD.string |> JD.map String.concat
        ]
