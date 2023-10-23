module Bytes.Extra exposing (..)

import Bytes exposing (Bytes, width)
import Bytes.Decode as D


{-| Break a Bytestring into a list of chunks. Chunks are of the given width,
except the last chunk which is only _at most_ the given width.
-}
chunksOf : Int -> Bytes -> List Bytes
chunksOf n bytes =
    D.decode
        (D.loop ( width bytes, [] ) <|
            \( w, chunks ) ->
                if w == 0 then
                    D.succeed (D.Done <| List.reverse chunks)

                else
                    let
                        len =
                            min w n
                    in
                    D.bytes len
                        |> D.map (\chunk -> D.Loop ( w - len, chunk :: chunks ))
        )
        bytes
        |> Maybe.withDefault []
