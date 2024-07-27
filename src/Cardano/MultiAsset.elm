module Cardano.MultiAsset exposing
    ( MultiAsset, PolicyId, AssetName
    , empty, isEmpty, onlyToken
    , coinsToCbor, mintToCbor, coinsFromCbor, mintFromCbor
    )

{-| Handling multi-asset values.

@docs MultiAsset, PolicyId, AssetName
@docs empty, isEmpty, onlyToken
@docs coinsToCbor, mintToCbor, coinsFromCbor, mintFromCbor

-}

import Bytes.Comparable exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cbor.Decode as D
import Cbor.Decode.Extra as DE
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Integer exposing (Integer)
import Natural exposing (Natural)


{-| Type alias for handling multi-asset values.

This type should maintain some invariants by construction.
In particular, it should never contain a zero quantity of a particular token.

TODO: make sure the previous statement stays true by construction.
This would require an opaque type for MultiAsset.

-}
type alias MultiAsset int =
    BytesMap PolicyId (BytesMap AssetName int)


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
empty : MultiAsset a
empty =
    Bytes.Map.empty


{-| Check if the [MultiAsset] contains no token.
-}
isEmpty : MultiAsset a -> Bool
isEmpty =
    Bytes.Map.isEmpty


{-| Create a singleton [MultiAsset].
-}
onlyToken : Bytes PolicyId -> Bytes AssetName -> int -> MultiAsset int
onlyToken policy name amount =
    Bytes.Map.singleton policy (Bytes.Map.singleton name amount)


{-| CBOR encoder for [MultiAsset] coins.
-}
coinsToCbor : MultiAsset Natural -> E.Encoder
coinsToCbor multiAsset =
    Bytes.Map.toCbor (Bytes.Map.toCbor EE.natural) multiAsset


{-| CBOR encoder for [MultiAsset] mints.
-}
mintToCbor : MultiAsset Integer -> E.Encoder
mintToCbor multiAsset =
    Bytes.Map.toCbor (Bytes.Map.toCbor EE.integer) multiAsset


{-| CBOR decoder for [MultiAsset] coins.
-}
coinsFromCbor : D.Decoder (MultiAsset Natural)
coinsFromCbor =
    Bytes.Map.fromCbor (Bytes.Map.fromCbor DE.natural)


{-| CBOR decoder for [MultiAsset] mints.
-}
mintFromCbor : D.Decoder (MultiAsset Integer)
mintFromCbor =
    Bytes.Map.fromCbor (Bytes.Map.fromCbor DE.integer)
