module Bytes.Extra exposing (..)

import Bytes exposing (Bytes, width)
import Bytes.Decode as D
import Bytes.Encode as E


bytes : List Int -> Bytes
bytes =
    List.map E.unsignedInt8 >> E.sequence >> E.encode


{-| Break a Bytestring into a list of chunks. Chunks are of the given width,
except the last chunk which is only _at most_ the given width.
-}
chunksOf : Int -> Bytes -> List Bytes
chunksOf n bs =
    D.decode
        (D.loop ( width bs, [] ) <|
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
        bs
        |> Maybe.withDefault []
