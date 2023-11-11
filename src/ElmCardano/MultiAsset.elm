module ElmCardano.MultiAsset exposing
    ( MultiAsset, PolicyId, AssetName
    , empty, isEmpty, toCbor
    )

{-| Handling multi-asset values.

@docs MultiAsset, PolicyId, AssetName
@docs empty, isEmpty, toCbor

-}

import BytesMap exposing (BytesMap)
import Cbor.Encode as E


{-| Type alias for handling multi-asset values.
-}
type alias MultiAsset =
    BytesMap PolicyId (BytesMap AssetName Int)


{-| Phantom type for 28-bytes policy id.
This is a Blacke2b-224 hash.
-}
type PolicyId
    = PolicyId Never


{-| Phantom type for asset names.
This is a free-form bytes array of length <= 32 bytes.
-}
type AssetName
    = AssetName Never


{-| Create an empty [MultiAsset].
-}
empty : MultiAsset
empty =
    BytesMap.empty


{-| Check if the [MultiAsset] contains no token.
-}
isEmpty : MultiAsset -> Bool
isEmpty =
    BytesMap.isEmpty


{-| CBOR encoder for [MultiAsset].
-}
toCbor : MultiAsset -> E.Encoder
toCbor multiAsset =
    BytesMap.toCbor (BytesMap.toCbor E.int) multiAsset
