module Blake2b.Int128 exposing (Int128(..), add, and, complement, decode, multiply, or, rotateRightBy, shiftLeftBy, shiftRightZfBy, toByteValues, toEncoder, toHex, toUnsigned, xor)

import Blake2b.Int64 as Int64 exposing (Int64(..))
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)


type Int128
    = Int128 Int64 Int64


and : Int128 -> Int128 -> Int128
and (Int128 a b) (Int128 p q) =
    Int128 (Int64.and a p) (Int64.and b q)


complement : Int128 -> Int128
complement (Int128 a b) =
    Int128
        (Int64.complement a |> Int64.shiftRightZfBy 0)
        (Int64.complement b |> Int64.shiftRightZfBy 0)


or : Int128 -> Int128 -> Int128
or (Int128 a b) (Int128 p q) =
    Int128 (Int64.or a p) (Int64.or b q)


xor : Int128 -> Int128 -> Int128
xor (Int128 a b) (Int128 p q) =
    Int128 (Int64.xor a p) (Int64.xor b q)


add : Int128 -> Int128 -> Int128
add (Int128 a b) (Int128 p q) =
    let
        ((Int64 lowerMS lowerLS) as lower) =
            Int64.add (Int64.shiftRightZfBy 0 b) (Int64.shiftRightZfBy 0 q)

        higher =
            Int64.add (Int64.shiftRightZfBy 0 a) (Int64.shiftRightZfBy 0 p)
    in
    -- check for overflow in the lower bits
    if lowerMS > 0xFFFFFFFF && lowerLS > 0xFFFFFFFF then
        Int128
            (Int64.shiftRightZfBy 0 (Int64.add higher <| Int64 0 1))
            (Int64.shiftRightZfBy 0 lower)

    else
        Int128 (Int64.shiftRightZfBy 0 higher) (Int64.shiftRightZfBy 0 lower)


shiftLeftBy : Int -> Int128 -> Int128
shiftLeftBy n (Int128 higher lower) =
    if n > 64 then
        let
            carry =
                Int64.shiftLeftBy n lower
        in
        Int128 carry (Int64 0 0)

    else
        let
            carry =
                Int64.shiftRightZfBy (64 - n) lower

            newHigher =
                higher
                    |> Int64.shiftLeftBy n
                    |> Int64.or carry
        in
        Int128 newHigher (Int64.shiftLeftBy n lower)


shiftRightZfBy : Int -> Int128 -> Int128
shiftRightZfBy n (Int128 higher lower) =
    if n > 64 then
        Int128 (Int64 0 0) (Int64.shiftRightZfBy n higher)

    else
        let
            carry =
                Int64.shiftLeftBy (64 - n) higher

            newLower =
                lower
                    |> Int64.shiftRightZfBy n
                    |> Int64.or carry
                    |> Int64.shiftRightZfBy 0
        in
        Int128 (Int64.shiftRightZfBy n higher) newLower


rotateRightBy : Int -> Int128 -> Int128
rotateRightBy n (Int128 higher lower) =
    if n > 64 then
        let
            -- guaranteed m <= 64
            m =
                128 - n

            carry =
                Int64.shiftRightZfBy (64 - m) lower

            p1 =
                higher
                    |> Int64.shiftLeftBy m
                    |> Int64.or carry

            p2 =
                Int64.shiftLeftBy m lower

            q1 =
                Int64 0 0

            q2 =
                Int64.shiftRightZfBy n higher
        in
        Int128 (Int64.or p1 q1) (Int64.or p2 q2)

    else
        let
            -- guaranteed n <= 64, m > 64
            m =
                128 - n

            p1 =
                Int64.shiftLeftBy m lower

            p2 =
                Int64 0 0

            carry =
                Int64.shiftLeftBy (64 - n) higher

            q1 =
                Int64.shiftRightZfBy n higher

            q2 =
                lower
                    |> Int64.shiftRightZfBy n
                    |> Int64.or carry
        in
        Int128 (Int64.or p1 q1) (Int64.or p2 q2)


multiplyHelper : Int128 -> Int128 -> Int128 -> Int128
multiplyHelper a b acc =
    let
        zero =
            Int64 0 0

        one =
            Int64 0 1
    in
    if b == Int128 zero zero then
        acc

    else if b == Int128 zero one then
        acc

    else
        let
            lsbOfBIsOne =
                and b (Int128 zero one) == Int128 zero one

            newA =
                shiftLeftBy 1 a

            newB =
                shiftRightZfBy 1 b
        in
        if lsbOfBIsOne then
            multiplyHelper (add a acc) newA newB

        else
            multiplyHelper acc newA newB


multiply : Int128 -> Int128 -> Int128
multiply a b =
    let
        zero =
            Int64 0 0
    in
    multiplyHelper a b (Int128 zero zero)


toUnsigned : Int128 -> Int128
toUnsigned (Int128 a b) =
    Int128 (Int64.shiftRightZfBy 0 a) (Int64.shiftRightZfBy 0 b)



-- Bytes


decode : Decoder Int128
decode =
    Decode.map2 Int128 Int64.decode Int64.decode


toEncoder : Int128 -> Encoder
toEncoder (Int128 higher lower) =
    Encode.sequence
        [ Int64.toEncoder higher
        , Int64.toEncoder lower
        ]


toHex : Int128 -> String
toHex (Int128 higher lower) =
    let
        high =
            higher
                |> Int64.toHex

        low =
            lower
                |> Int64.toHex
    in
    high ++ low


toByteValues : Int128 -> List Int
toByteValues (Int128 higher lower) =
    Int64.toByteValues higher ++ Int64.toByteValues lower
