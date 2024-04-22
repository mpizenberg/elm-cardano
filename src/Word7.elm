module Word7 exposing (fromBytes)

{-| Decoding 7-bits words.

@docs fromBytes

-}

import Bitwise exposing (and, or, shiftLeftBy)
import Bytes.Decode as D


{-| Decode a 7-bit encoded integer
-}
fromBytes : D.Decoder Int
fromBytes =
    let
        word7sToInt =
            List.foldl (\w i -> i |> shiftLeftBy 7 |> or w) 0
    in
    D.map word7sToInt getWord7s


getWord7s : D.Decoder (List Int)
getWord7s =
    D.unsignedInt8
        |> D.andThen
            (\i ->
                case and i 0x80 of
                    0x80 ->
                        getWord7s |> D.map (\tail -> i :: tail)

                    _ ->
                        D.succeed [ i ]
            )
