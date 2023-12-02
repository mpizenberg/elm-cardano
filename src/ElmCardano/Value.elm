module ElmCardano.Value exposing (Value, onlyLovelace, encode)

{-| Handling Cardano values.

@docs Value, onlyLovelace, encode

-}

import Cbor.Decode as D
import Cbor.Encode as E
import ElmCardano.MultiAsset as MultiAsset exposing (MultiAsset)


{-| A multi-asset output Value. Contains tokens indexed by policy id and asset name.

This type maintains some invariants by construction.
In particular, a Value will never contain a zero quantity of a particular token.

TODO: make sure the previous statement stays true?

-}
type alias Value =
    { lovelace : Int, assets : MultiAsset }


{-| Create a [Value] just containing Ada lovelaces.
-}
onlyLovelace : Int -> Value
onlyLovelace lovelace =
    { lovelace = lovelace, assets = MultiAsset.empty }


{-| CBOR encoder for [Value].
-}
encode : Value -> E.Encoder
encode { lovelace, assets } =
    if MultiAsset.isEmpty assets then
        E.int lovelace

    else
        E.sequence
            [ E.beginList
            , E.int lovelace
            , MultiAsset.toCbor assets
            , E.break
            ]


decode : D.Decoder Value
decode =
    D.fail
