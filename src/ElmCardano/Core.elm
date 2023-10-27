module ElmCardano.Core exposing (NetworkId(..), PosixTime)

{-| Essential elements for Cardano smart contracts that don't have a dedicated module.
-}


{-| Number of milliseconds since 00:00:00 UTC on 1 January 1970.
-}
type PosixTime
    = PosixTime Int


type NetworkId
    = Testnet -- 0
    | Mainnet -- 1
