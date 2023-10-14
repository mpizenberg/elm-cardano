module ElmCardano.Core exposing (Coin, Data(..), NetworkId(..), PosixTime)

{-| Essential elements for Cardano smart contracts that don't have a dedicated module.
-}


{-| Number of milliseconds since 00:00:00 UTC on 1 January 1970.
-}
type PosixTime
    = PosixTime Int


type NetworkId
    = One -- 0
    | Two -- 1


type alias Coin =
    Int


{-| A Data is an opaque compound type that can represent any possible user-defined type in Aiken.
-}
type Data
    = Constr
        { tag : Int
        , anyConstructor : Maybe Int
        , fields : List Data
        }
