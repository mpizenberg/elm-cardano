module Cardano.Interval exposing (Interval, Bound, BoundType(..))

{-| In a eUTxO-based blockchain like Cardano, the management of time can be finicky.
Indeed, in order to maintain a complete determinism in the execution of scripts, it is impossible to introduce a notion of "current time".
The execution would then depend on factor that are external to the transaction itself: the ineluctable stream of time flowing in our universe.
Hence, to work around that, we typically define time intervals, within which the transaction can be executed.
From within a script, it isn’t possible to know when exactly the script is executed, but we can reason about the interval bounds to validate pieces of logic.

@docs Interval, Bound, BoundType

-}


{-| A type to represent intervals of values, with finite or infinite bounds.
-}
type alias Interval a =
    { lowerBound : Bound a
    , upperBound : Bound a
    }


{-| Bound of an interval.
-}
type alias Bound a =
    { boundType : BoundType a
    , isInclusive : Bool
    }


{-| The bound type, which can be finite or infinite.
-}
type BoundType a
    = NegativeInfinity
    | Finite a
    | PositiveInfinity
