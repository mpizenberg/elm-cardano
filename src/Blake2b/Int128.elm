module Blake2b.Int128 exposing
    ( Int128(..)
    , add
    , and
    , complement
    , mul
    , or
    , rotateRightBy
    , shiftLeftBy
    , shiftRightZfBy
    , toHex
    , toUnsigned
    , xor
    )

import Bytes exposing (Endianness(..))
import UInt64 exposing (UInt64)


type Int128
    = Int128 UInt64 UInt64


and : Int128 -> Int128 -> Int128
and (Int128 a b) (Int128 p q) =
    Int128 (UInt64.and a p) (UInt64.and b q)


complement : Int128 -> Int128
complement (Int128 a b) =
    Int128
        (UInt64.complement a |> UInt64.shiftRightZfBy 0)
        (UInt64.complement b |> UInt64.shiftRightZfBy 0)


or : Int128 -> Int128 -> Int128
or (Int128 a b) (Int128 p q) =
    Int128 (UInt64.or a p) (UInt64.or b q)


xor : Int128 -> Int128 -> Int128
xor (Int128 a b) (Int128 p q) =
    Int128 (UInt64.xor a p) (UInt64.xor b q)


add : Int128 -> Int128 -> Int128
add (Int128 a b) (Int128 p q) =
    let
        lower =
            UInt64.add b q

        higher =
            UInt64.add a p
    in
    -- check for overflow in the lower bits
    case UInt64.compare lower UInt64.maxValue of
        GT ->
            Int128
                (UInt64.add higher UInt64.one)
                lower

        _ ->
            Int128 higher lower


shiftLeftBy : Int -> Int128 -> Int128
shiftLeftBy n (Int128 higher lower) =
    if n > 64 then
        let
            carry =
                UInt64.shiftLeftBy n lower
        in
        Int128 carry UInt64.zero

    else
        let
            carry =
                UInt64.shiftRightZfBy (64 - n) lower

            newHigher =
                higher
                    |> UInt64.shiftLeftBy n
                    |> UInt64.or carry
        in
        Int128 newHigher (UInt64.shiftLeftBy n lower)


shiftRightZfBy : Int -> Int128 -> Int128
shiftRightZfBy n (Int128 higher lower) =
    if n > 64 then
        Int128 UInt64.zero (UInt64.shiftRightZfBy n higher)

    else
        let
            carry =
                UInt64.shiftLeftBy (64 - n) higher

            newLower =
                lower
                    |> UInt64.shiftRightZfBy n
                    |> UInt64.or carry
                    |> UInt64.shiftRightZfBy 0
        in
        Int128 (UInt64.shiftRightZfBy n higher) newLower


rotateRightBy : Int -> Int128 -> Int128
rotateRightBy n (Int128 higher lower) =
    if n > 64 then
        let
            -- guaranteed m <= 64
            m =
                128 - n

            carry =
                UInt64.shiftRightZfBy (64 - m) lower

            p1 =
                higher
                    |> UInt64.shiftLeftBy m
                    |> UInt64.or carry

            p2 =
                UInt64.shiftLeftBy m lower

            q1 =
                UInt64.zero

            q2 =
                UInt64.shiftRightZfBy n higher
        in
        Int128 (UInt64.or p1 q1) (UInt64.or p2 q2)

    else
        let
            -- guaranteed n <= 64, m > 64
            m =
                128 - n

            p1 =
                UInt64.shiftLeftBy m lower

            p2 =
                UInt64.zero

            carry =
                UInt64.shiftLeftBy (64 - n) higher

            q1 =
                UInt64.shiftRightZfBy n higher

            q2 =
                lower
                    |> UInt64.shiftRightZfBy n
                    |> UInt64.or carry
        in
        Int128 (UInt64.or p1 q1) (UInt64.or p2 q2)


mul : UInt64 -> UInt64 -> Int128
mul a b =
    case ( UInt64.toInt31 a, UInt64.toInt31 b ) of
        ( Just a0, Just b0 ) ->
            Int128 UInt64.zero (UInt64.fromInt (a0 * b0))

        _ ->
            let
                ( aLow, aHigh ) =
                    UInt64.toInt32s a

                ( bLow, bHigh ) =
                    UInt64.toInt32s b

                aLow64 =
                    UInt64.fromInt aLow

                aHigh64 =
                    UInt64.fromInt aHigh

                bLow64 =
                    UInt64.fromInt bLow

                bHigh64 =
                    UInt64.fromInt bHigh

                z0 =
                    UInt64.mul aLow64 bLow64

                z1 =
                    UInt64.mul aLow64 bHigh64

                z2 =
                    UInt64.mul aHigh64 bLow64

                z3 =
                    UInt64.mul aHigh64 bHigh64

                shift32 =
                    UInt64.shiftRightZfBy 32

                lsb32 =
                    UInt64.and (UInt64.fromInt 0xFFFFFFFF)

                lo =
                    z0

                hi =
                    UInt64.add
                        z3
                        (UInt64.add
                            (shift32 z1)
                            (shift32 z2)
                        )

                t =
                    UInt64.add (shift32 z0) (UInt64.add (lsb32 z1) (lsb32 z2))
            in
            Int128
                (UInt64.add lo (UInt64.shiftLeftBy 32 (lsb32 t)))
                (UInt64.add hi (shift32 t))


toUnsigned : Int128 -> Int128
toUnsigned (Int128 a b) =
    Int128 (UInt64.shiftRightZfBy 0 a) (UInt64.shiftRightZfBy 0 b)


toHex : Int128 -> String
toHex (Int128 higher lower) =
    let
        high =
            higher
                |> UInt64.toHexString
                |> String.toLower

        low =
            lower
                |> UInt64.toHexString
                |> String.toLower
    in
    high ++ low
