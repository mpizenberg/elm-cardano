module ElmCardano.MultiAsset exposing
    ( MultiAsset
    , empty, isEmpty
    , encode
    )

{-| Handling multi-asset values.

@docs MultiAsset
@docs empty, isEmpty
@docs encode

-}

import Cbor.Encode as E
import Dict exposing (Dict)


{-| Opaque type for handling multi-asset values.
-}
type MultiAsset
    = MultiAsset (Dict String (Dict String Int)) -- Dict PolicyId (Dict AssetName Int)


{-| Create an empty multi-asset value.
-}
empty : MultiAsset
empty =
    MultiAsset Dict.empty


{-| Check if a multi-asset value is empty.
-}
isEmpty : MultiAsset -> Bool
isEmpty (MultiAsset assets) =
    Dict.isEmpty assets


{-| CBOR encoder for [MultiAsset].
-}
encode : MultiAsset -> E.Encoder
encode (MultiAsset _) =
    Debug.todo "MultiAsset.encode"
