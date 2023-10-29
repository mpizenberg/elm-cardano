module Hex.Extra exposing
    ( Hex
    , fromString
    , toHexString
    , toBytes
    , toUTF8String
    )

{-| Providing robust hex string smart constructors/deconstructors.

@docs Hex, fromString, toHexString, toUTF8String, toBytes

-}

import Bytes exposing (Bytes)
import Bytes.Encode as BE
import Hex.Convert as HexConvert
import Hex as HexBasic
import String.UTF8 as UTF8


{-| An opaque 'String' wrapper to ensure the underlying 'String' is a hex.
-}
type Hex =
    Hex String


{-| Smart constructor for the 'Hex' datatype. If the given 'String' is not in
hex, it'll implicitly assume it as a UTF-8 encoded string, convert each
character to its hex equivalent, and wrap the resulting string into a 'Hex'.
-}
fromString : String -> Hex
fromString str =
    case HexConvert.toBytes str of
        Nothing ->
            UTF8.toBytes str
                |> List.map HexBasic.toString
                |> String.concat
                |> Hex

        Just _ ->
            Hex str


{-| Unwrapper function for accessing the 'String' underneath.
-}
toHexString : Hex -> String
toHexString hex =
    case hex of
        Hex str ->
            str


{-| A convenient helper function for UTF-8 decoding a 'Hex'. Usefult for
Cardano token names.
-}
toUTF8String : Hex -> String
toUTF8String hex =
    case UTF8.toString (UTF8.toBytes <| toHexString hex) of
        Ok str ->
            str

        Err _ ->
            -- Another impossible case.
            toHexString hex


{-| For converting a 'Hex' into 'Bytes'.
-}
toBytes : Hex -> Bytes
toBytes hex =
    case HexConvert.toBytes (toHexString hex) of
        Nothing ->
            -- `Hex` can only be created with `fromString`, therefore this is
            -- an impossible case and most likely an acceptable compromise.
            BE.encode <| BE.unsignedInt8 0

        Just hexBytes ->
            hexBytes
