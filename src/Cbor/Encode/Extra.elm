module Cbor.Encode.Extra exposing (..)

import Cbor.Encode as E


{-| Encode a foldable only if non empty.
-}
nonEmptyField :
    k
    -> (field -> Bool)
    -> (field -> E.Encoder)
    -> (record -> field)
    -> E.Step k record
    -> E.Step k record
nonEmptyField key isEmpty encode extract =
    E.optionalField key encode <|
        extract
            >> (\xs ->
                    if isEmpty xs then
                        Nothing

                    else
                        Just xs
               )


{-| Optionally encode an element in a tuple
-}
optionalElem :
    (elem -> E.Encoder)
    -> Maybe elem
    -> E.Step Never tuple
    -> E.Step Never tuple
optionalElem encode maybe =
    case maybe of
        Nothing ->
            identity

        Just e ->
            E.elem encode (always e)
