module ElmCardano.MultiAsset exposing (MultiAsset, empty, encode, isEmpty)

{-| Handling values.
-}

import Cbor.Encode as E
import Dict exposing (Dict)


type MultiAsset
    = MultiAsset (Dict String (Dict String Int)) -- Dict PolicyId (Dict AssetName Int)


empty : MultiAsset
empty =
    MultiAsset Dict.empty


isEmpty : MultiAsset -> Bool
isEmpty (MultiAsset assets) =
    Dict.isEmpty assets


encode : MultiAsset -> E.Encoder
encode (MultiAsset _) =
    Debug.todo "MultiAsset.encode"
