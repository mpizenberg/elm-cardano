module ElmCardano.Core exposing (NetworkId(..))

{-| Essential elements for Cardano smart contracts that don't have a dedicated module.
-}


type NetworkId
    = Testnet -- 0
    | Mainnet -- 1
