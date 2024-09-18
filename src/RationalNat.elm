module RationalNat exposing
    ( RationalNat
    , zero, fromSafeInt
    , add, mul, floor
    )

{-| Unbounded positive rational numbers,
based on [Natural] numbers for the fraction.

@docs RationalNat

@docs zero, fromSafeInt

@docs add, mul, floor

-}

import Natural exposing (Natural)


{-| Unbounded positive rational numbers,
based on [Natural] numbers for both sides of the fraction.
-}
type alias RationalNat =
    { num : Natural
    , denom : Natural
    }


{-| 0
-}
zero : RationalNat
zero =
    fromSafeInt 0


{-| Convert from a safe JS integer (< 2^53) to [RationalNat],
using 1 for the denominator.

This has the same limitations than the [Natural] function with the same name.

-}
fromSafeInt : Int -> RationalNat
fromSafeInt int =
    { num = Natural.fromSafeInt int
    , denom = Natural.one
    }


{-| Addition

Remark that the denominator part will grow due to exact computation.
Not simplification is performed.

-}
add : RationalNat -> RationalNat -> RationalNat
add r1 r2 =
    { num = Natural.add (Natural.mul r1.num r2.denom) (Natural.mul r2.num r1.denom)
    , denom = Natural.mul r1.denom r2.denom
    }


{-| Multiplication

Remark that the denominator part will grow due to exact computation.
Not simplification is performed.

-}
mul : RationalNat -> RationalNat -> RationalNat
mul r1 r2 =
    { num = Natural.mul r1.num r2.num
    , denom = Natural.mul r1.denom r2.denom
    }


{-| Return the integer part of that rational number.

Return [Nothing] if the denominator is 0.

-}
floor : RationalNat -> Maybe Natural
floor r =
    r.num |> Natural.divBy r.denom
