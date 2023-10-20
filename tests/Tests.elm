module Tests exposing (..)

import Bytes exposing (Bytes, width)
import Bytes.Decode as D
import Bytes.Encode as E
import Hex.Convert


{-| Convert a list of BE unsigned8 to bytes
-}
hex : Bytes -> Maybe (List Int)
hex bytes =
    bytes
        |> D.decode
            (D.loop ( width bytes, [] )
                (\( n, xs ) ->
                    if n == 0 then
                        xs |> List.reverse |> D.Done |> D.succeed

                    else
                        D.unsignedInt8
                            |> D.map (\x -> D.Loop ( n - 1, x :: xs ))
                )
            )


{-| Convert a list of BE unsigned8 to bytes
-}
toBytes : List Int -> Bytes
toBytes =
    List.map E.unsignedInt8 >> E.sequence >> E.encode


fromString : String -> Bytes
fromString s =
    case Hex.Convert.toBytes s of
        Just x ->
            x

        Nothing ->
            toBytes [ 0x00 ]
