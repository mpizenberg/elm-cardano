module String.Extra exposing (chunksOf)

{-| Extra [String] utility functions.
-}


{-| Helper function for breaking down long strings into smaller chunks.

Returns a list of chunks where the last element is at most the length of the
given chunk size.

-}
chunksOf : Int -> String -> List String
chunksOf chunkSize str =
    chunksOfHelper chunkSize str []


{-| Helper function for `chunksOf` to allow tail call optimization.
-}
chunksOfHelper : Int -> String -> List String -> List String
chunksOfHelper chunkSize remainingStr acc =
    if remainingStr == "" then
        List.reverse acc

    else
        chunksOfHelper
            chunkSize
            (String.dropLeft chunkSize remainingStr)
            (String.left chunkSize remainingStr :: acc)
