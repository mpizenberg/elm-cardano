module ElmCardano.Value exposing (..)

{-| Handling values.
-}

import Bytes exposing (Bytes)
import BytesMap exposing (BytesMap)
import Cbor.Encode as E
import ElmCardano.Core exposing (Coin)
import ElmCardano.Hash exposing (Blake2b_224)


{-| A multi-asset output Value. Contains tokens indexed by policy id and asset name.

This type maintains some invariants by construction.
In particular, a Value will never contain a zero quantity of a particular token.

-}
type Value
    = Coin Coin
    | Multiasset Coin (Multiasset Coin)


type alias Multiasset a =
    BytesMap PolicyId (BytesMap AssetName a)


{-| The policy id of a Cardano Token. Ada ("") is a special case since it cannot be minted.
-}
type alias PolicyId =
    Blake2b_224


type alias AssetName =
    Bytes


{-| Ada, the native currency, isn’t associated with any AssetName (it’s not possible to mint Ada!).
By convention, it is an empty ByteArray.
-}
adaAssetName : String
adaAssetName =
    ""


encodeValue : Value -> E.Encoder
encodeValue value =
    case value of
        Coin amount ->
            E.int amount

        Multiasset amount _ ->
            E.sequence
                [ E.beginList
                , E.int amount
                , Debug.todo "encode multiasset"
                , E.break
                ]
