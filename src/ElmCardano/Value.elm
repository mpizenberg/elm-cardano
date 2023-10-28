module ElmCardano.Value exposing (Value, encode, onlyLovelace)

{-| Handling values.
-}

import Cbor.Encode as E
import ElmCardano.MultiAsset as MultiAsset exposing (MultiAsset)


{-| A multi-asset output Value. Contains tokens indexed by policy id and asset name.

This type maintains some invariants by construction.
In particular, a Value will never contain a zero quantity of a particular token.

-}
type alias Value =
    { lovelace : Int, assets : MultiAsset }


onlyLovelace : Int -> Value
onlyLovelace lovelace =
    { lovelace = lovelace, assets = MultiAsset.empty }


encode : Value -> E.Encoder
encode { lovelace, assets } =
    if MultiAsset.isEmpty assets then
        E.int lovelace

    else
        E.sequence
            [ E.beginList
            , E.int lovelace
            , MultiAsset.encode assets
            , E.break
            ]
