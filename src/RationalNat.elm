module RationalNat exposing (..)

import Natural exposing (Natural)


type alias RationalNat =
    { num : Natural
    , denom : Natural
    }


zero : RationalNat
zero =
    fromSafeInt 0


fromSafeInt : Int -> RationalNat
fromSafeInt int =
    { num = Natural.fromSafeInt int
    , denom = Natural.one
    }


add : RationalNat -> RationalNat -> RationalNat
add r1 r2 =
    { num = Natural.add (Natural.mul r1.num r2.denom) (Natural.mul r2.num r1.denom)
    , denom = Natural.mul r1.denom r2.denom
    }


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
