module Cardano.Value exposing (Value, onlyLovelace, encode, fromCbor)

{-| Handling Cardano values.

@docs Value, onlyLovelace, encode, fromCbor

-}

import Cardano.MultiAsset as MultiAsset exposing (MultiAsset)
import Cbor.Decode as D
import Cbor.Decode.Extra as DE
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Natural exposing (Natural)


{-| A multi-asset output Value. Contains tokens indexed by policy id and asset name.

This type maintains some invariants by construction.
In particular, a Value will never contain a zero quantity of a particular token.

TODO: make sure the previous statement stays true?

-}
type alias Value =
    { lovelace : Natural, assets : MultiAsset Natural }


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
            , MultiAsset.coinsToCbor assets
            , E.break
            ]


{-| CBOR decoder for [Value].
-}
fromCbor : D.Decoder Value
fromCbor =
    D.oneOf
        -- value = coin / [coin,multiasset<uint>]
        [ D.map onlyLovelace DE.natural
        , D.tuple Value <|
            D.elems
                >> D.elem DE.natural
                >> D.elem MultiAsset.coinsFromCbor
        ]
