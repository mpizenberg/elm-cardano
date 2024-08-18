module Blake2b.Int64 exposing (Int64(..), add, and, complement, decode, or, rotateRightBy, shiftLeftBy, shiftRightZfBy, toByteValues, toEncoder, toHex, toUnsigned, xor)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Hex.Convert as Hex


type Int64
    = Int64 Int Int


and : Int64 -> Int64 -> Int64
and (Int64 a b) (Int64 p q) =
    Int64 (Bitwise.and a p) (Bitwise.and b q)


complement : Int64 -> Int64
complement (Int64 a b) =
    Int64
        (Bitwise.complement a |> Bitwise.shiftRightZfBy 0)
        (Bitwise.complement b |> Bitwise.shiftRightZfBy 0)


or : Int64 -> Int64 -> Int64
or (Int64 a b) (Int64 p q) =
    Int64 (Bitwise.or a p) (Bitwise.or b q)


xor : Int64 -> Int64 -> Int64
xor (Int64 a b) (Int64 p q) =
    Int64 (Bitwise.xor a p) (Bitwise.xor b q)


add : Int64 -> Int64 -> Int64
add (Int64 a b) (Int64 p q) =
    let
        lower =
            Bitwise.shiftRightZfBy 0 b + Bitwise.shiftRightZfBy 0 q

        higher =
            Bitwise.shiftRightZfBy 0 a + Bitwise.shiftRightZfBy 0 p
    in
    -- check for overflow in the lower bits
    if lower > 0xFFFFFFFF then
        Int64 (Bitwise.shiftRightZfBy 0 (higher + 1)) (Bitwise.shiftRightZfBy 0 lower)

    else
        Int64 (Bitwise.shiftRightZfBy 0 higher) (Bitwise.shiftRightZfBy 0 lower)


shiftLeftBy : Int -> Int64 -> Int64
shiftLeftBy n (Int64 higher lower) =
    if n > 32 then
        let
            carry =
                Bitwise.shiftLeftBy n lower
        in
        Int64 carry 0

    else
        let
            carry =
                Bitwise.shiftRightZfBy (32 - n) lower

            newHigher =
                higher
                    |> Bitwise.shiftLeftBy n
                    |> Bitwise.or carry
        in
        Int64 newHigher (Bitwise.shiftLeftBy n lower)


shiftRightZfBy : Int -> Int64 -> Int64
shiftRightZfBy n (Int64 higher lower) =
    if n > 32 then
        Int64 0 (Bitwise.shiftRightZfBy n higher)

    else
        let
            carry =
                Bitwise.shiftLeftBy (32 - n) higher

            newLower =
                lower
                    |> Bitwise.shiftRightZfBy n
                    |> Bitwise.or carry
                    |> Bitwise.shiftRightZfBy 0
        in
        Int64 (Bitwise.shiftRightZfBy n higher) newLower


rotateRightBy : Int -> Int64 -> Int64
rotateRightBy n (Int64 higher lower) =
    if n > 32 then
        let
            -- guaranteed m <= 32
            m =
                64 - n

            carry =
                Bitwise.shiftRightZfBy (32 - m) lower

            p1 =
                higher
                    |> Bitwise.shiftLeftBy m
                    |> Bitwise.or carry

            p2 =
                Bitwise.shiftLeftBy m lower

            q1 =
                0

            q2 =
                Bitwise.shiftRightZfBy n higher
        in
        Int64 (Bitwise.or p1 q1) (Bitwise.or p2 q2)

    else
        let
            -- guaranteed n <= 32, m > 32
            m =
                64 - n

            p1 =
                Bitwise.shiftLeftBy m lower

            p2 =
                0

            carry =
                Bitwise.shiftLeftBy (32 - n) higher

            q1 =
                Bitwise.shiftRightZfBy n higher

            q2 =
                lower
                    |> Bitwise.shiftRightZfBy n
                    |> Bitwise.or carry
        in
        Int64 (Bitwise.or p1 q1) (Bitwise.or p2 q2)


toUnsigned : Int64 -> Int64
toUnsigned (Int64 a b) =
    Int64 (Bitwise.shiftRightZfBy 0 a) (Bitwise.shiftRightZfBy 0 b)



-- Bytes


decode : Decoder Int64
decode =
    Decode.map2 Int64
        (Decode.unsignedInt32 BE)
        (Decode.unsignedInt32 BE)


toEncoder : Int64 -> Encoder
toEncoder (Int64 higher lower) =
    Encode.sequence
        [ Encode.unsignedInt32 BE higher
        , Encode.unsignedInt32 BE lower
        ]


toHex : Int64 -> String
toHex (Int64 higher lower) =
    let
        int32ToBytes x =
            Encode.encode (Encode.unsignedInt32 BE x)

        high =
            higher
                |> Bitwise.shiftRightZfBy 0
                |> int32ToBytes
                |> Hex.toString
                |> String.padLeft 8 '0'

        low =
            lower
                |> Bitwise.shiftRightZfBy 0
                |> int32ToBytes
                |> Hex.toString
                |> String.padLeft 8 '0'
    in
    high ++ low


toByteValues : Int64 -> List Int
toByteValues (Int64 higher lower) =
    wordToBytes higher ++ wordToBytes lower


wordToBytes : Int -> List Int
wordToBytes int =
    [ int |> Bitwise.shiftRightZfBy 0x18 |> Bitwise.and 0xFF
    , int |> Bitwise.shiftRightZfBy 0x10 |> Bitwise.and 0xFF
    , int |> Bitwise.shiftRightZfBy 0x08 |> Bitwise.and 0xFF
    , int |> Bitwise.and 0xFF
    ]
