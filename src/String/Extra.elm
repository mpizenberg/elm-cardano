module String.Extra exposing (chunksOf)

{-| Extra [String] utility functions.
-}


{-| Helper function for breaking down long strings into smaller chunks.

This implementation cuts off the last chunk if the given string is not
divisible by the given chunk size.

-}
chunksOf : Int -> String -> List String
chunksOf chunkSize str =
    let
        go remainingStr acc =
            if remainingStr == "" then
                List.reverse acc

            else
                go
                    (String.dropLeft chunkSize remainingStr)
                    (String.left chunkSize remainingStr :: acc)
    in
    go str []
