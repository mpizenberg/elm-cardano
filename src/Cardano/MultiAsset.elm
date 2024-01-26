module Cardano.MultiAsset exposing
    ( MultiAsset, PolicyId, AssetName
    , empty, isEmpty, toCbor
    )

{-| Handling multi-asset values.

@docs MultiAsset, PolicyId, AssetName
@docs empty, isEmpty, toCbor

-}

import Bytes.Comparable exposing (Any, Bytes)
import Bytes.Fixed exposing (Bytes28)
import Bytes.Map exposing (BytesMap)
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Natural exposing (Natural)


{-| Type alias for handling multi-asset values.
-}
type alias MultiAsset =
    BytesMap PolicyId (BytesMap AssetName Natural)


{-| Phantom type for 28-bytes policy id.
This is a Blacke2b-224 hash.
-}
type alias PolicyId =
    Bytes28


{-| Phantom type for asset names.
This is a free-form bytes array of length <= 32 bytes.
-}
type AssetName
    = Bytes Any


{-| Create an empty [MultiAsset].
-}
empty : MultiAsset
empty =
    Bytes.Map.empty


{-| Check if the [MultiAsset] contains no token.
-}
isEmpty : MultiAsset -> Bool
isEmpty =
    Bytes.Map.isEmpty


{-| CBOR encoder for [MultiAsset].
-}
toCbor : MultiAsset -> E.Encoder
toCbor multiAsset =
    Bytes.Map.toCbor (Bytes.Map.toCbor EE.natural) multiAsset
