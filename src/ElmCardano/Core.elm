module ElmCardano.Core exposing (Data, PosixTime)

{-| Essential elements for Cardano smart contracts that don't have a dedicated module.
-}


{-| Number of milliseconds since 00:00:00 UTC on 1 January 1970.
-}
type PosixTime
    = PosixTime Int


{-| A Data is an opaque compound type that can represent any possible user-defined type in Aiken.
-}
type Data
    = Data
