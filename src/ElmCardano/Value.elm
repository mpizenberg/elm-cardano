module ElmCardano.Value exposing (Value, onlyLovelace, encode)

{-| Handling Cardano values.

@docs Value, onlyLovelace, encode

-}

import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import ElmCardano.MultiAsset as MultiAsset exposing (MultiAsset)
import Natural exposing (Natural)


{-| A multi-asset output Value. Contains tokens indexed by policy id and asset name.

This type maintains some invariants by construction.
In particular, a Value will never contain a zero quantity of a particular token.

TODO: make sure the previous statement stays true?

-}
type alias Value =
    { lovelace : Natural, assets : MultiAsset }


{-| Create a [Value] just containing Ada lovelaces.
-}
onlyLovelace : Natural -> Value
onlyLovelace lovelace =
    { lovelace = lovelace, assets = MultiAsset.empty }


{-| CBOR encoder for [Value].
-}
encode : Value -> E.Encoder
encode { lovelace, assets } =
    if MultiAsset.isEmpty assets then
        EE.natural lovelace

    else
        E.sequence
            [ E.beginList
            , EE.natural lovelace
            , MultiAsset.toCbor assets
            , E.break
            ]
