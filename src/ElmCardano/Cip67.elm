module ElmCardano.Cip67 exposing
    ( Cip67
    , fromBytes, fromCbor
    , toBytes, toCbor
    )

{-| CIP-0067 support.

CIP-0067 [describes](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0067)
a standard format for assets so that their purpose can be deduced solely by
their token names. So far, the primary application of this standard is to
easily distinguish between CIP-0068 assets and their reference counterparts.

@docs Cip67

@docs fromBytes, fromCbor

@docs toBytes, toCbor

-}

import Array exposing (Array)
import Bitwise
import Bytes.Comparable as Bytes exposing (Bytes)
import Cbor.Decode as D
import Cbor.Encode as E


{-| Datatype for modeling CIP-0067.

This standard offers a label number preceding the actual token name, which can
be from `0` to `65535` (i.e. 2 bytes, and left-padded with `00` for numbers
smaller than 2 bytes). This label should be formatted as such:

    --  openning bracket                             closing bracket
    --       ┌──┐                                         ┌──┐
    --     [ 0000 | 16 bits label_num | 8 bits checksum | 0000 ]
    --              └───────────────┘   └───────┬─────┘
    --        fixed 2 bytes for the label       │
    --                                          │
    -- label's checksum found by applying the CRC-8 algorithm to its 2 bytes



The [polynomial representation](https://wikipedia.org/wiki/Mathematics_of_cyclic_redundancy_checks#Polynomial_representations)
of the CRC-8 is `0x07` (`0b00000111`).

As an example, a label of `222` turn into: `0x000de140`

1.  Hex equivalent of decimal `222` is `0xde`
2.  Since it's less than 2 bytes, we add `0x00` to the left and get `0x00de`
3.  Find its CRC-8 checksum (`0x14`)
4.  Concatenate the two and wrap in "brackets" to get the final bytes: `0x000de140`

Finally, a complete CIP-0067 example:


    spacebudz2921 : Maybe Cip67
    spacebudz2921 =
        Maybe.andThen Cip67.fromBytes <| Bytes.fromString "000de14042756432393231"

    -- Just { assetName = Bytes "42756432393231", label = 222 }

-}
type alias Cip67 =
    { label : Int
    , assetName : Bytes
    }


{-| Validate and separate the label of a CIP-0067 asset name.

Given a valid CIP-0067 token name [Bytes], this function separates the label as
an [Int], and returns the asset name without the label bytes.

-}
fromBytes : Bytes -> Maybe Cip67
fromBytes tnBytes =
    let
        tnString =
            Bytes.toString tnBytes

        fullLabelStr =
            String.left 8 tnString

        hasBracketsAndProperLength =
            String.length fullLabelStr
                == 8
                && String.startsWith "0" fullLabelStr
                && String.endsWith "0" fullLabelStr
    in
    if hasBracketsAndProperLength then
        let
            labelWithChecksum =
                String.slice 1 -1 fullLabelStr

            labelBytes =
                Bytes.fromStringUnchecked <| String.left 4 labelWithChecksum

            checksumBytes =
                Bytes.fromStringUnchecked <| String.right 2 labelWithChecksum
        in
        if crc8 labelBytes == checksumBytes then
            Just
                { label = Bytes.toDecimal labelBytes
                , assetName = Bytes.fromStringUnchecked <| String.dropLeft 8 tnString
                }

        else
            Nothing

    else
        Nothing


{-| CBOR decoder for [Cip67].
-}
fromCbor : D.Decoder Cip67
fromCbor =
    -- TODO
    D.fail


{-| Converts a [Cip67] to [Bytes].
-}
toBytes : Cip67 -> Bytes
toBytes cip67 =
    let
        labelBytes =
            Bytes.fromDecimal cip67.label
                |> Bytes.toString
                |> String.padLeft 4 '0'
                |> Bytes.fromStringUnchecked

        checksumStr =
            Bytes.toString <| crc8 labelBytes

        tnStr =
            Bytes.toString cip67.assetName
    in
    "0"
        ++ Bytes.toString labelBytes
        ++ checksumStr
        ++ "0"
        ++ tnStr
        |> Bytes.fromStringUnchecked


{-| CBOR encoder.
-}
toCbor : Cip67 -> E.Encoder
toCbor =
    toBytes >> Bytes.toCbor


{-| Function for finding the CRC-8 digest of a given [`Bytes`].

Taken from [Haskell's `crc` library](https://hackage.haskell.org/package/crc-0.1.1.1/docs/src/Data.Digest.CRC8.html#updateDigest8), this
implementation uses the table lookup optimizatino for finding the digest.

-}
crc8 : Bytes -> Bytes
crc8 bs =
    let
        go : Int -> Int -> Int
        go w8 digest =
            let
                idx =
                    Bitwise.and (Bitwise.xor digest w8) 0xFF
            in
            case Array.get idx crc8Table of
                Just d ->
                    Bitwise.and (Bitwise.shiftRightBy 8 digest) 0xFF
                        |> Bitwise.xor d

                Nothing ->
                    digest
    in
    Bytes.fromDecimal <| List.foldl go 0 <| Bytes.toWord8s bs


crc8Table : Array Int
crc8Table =
    Array.fromList
        [ 0x00
        , 0x07
        , 0x0E
        , 0x09
        , 0x1C
        , 0x1B
        , 0x12
        , 0x15
        , 0x38
        , 0x3F
        , 0x36
        , 0x31
        , 0x24
        , 0x23
        , 0x2A
        , 0x2D
        , 0x70
        , 0x77
        , 0x7E
        , 0x79
        , 0x6C
        , 0x6B
        , 0x62
        , 0x65
        , 0x48
        , 0x4F
        , 0x46
        , 0x41
        , 0x54
        , 0x53
        , 0x5A
        , 0x5D
        , 0xE0
        , 0xE7
        , 0xEE
        , 0xE9
        , 0xFC
        , 0xFB
        , 0xF2
        , 0xF5
        , 0xD8
        , 0xDF
        , 0xD6
        , 0xD1
        , 0xC4
        , 0xC3
        , 0xCA
        , 0xCD
        , 0x90
        , 0x97
        , 0x9E
        , 0x99
        , 0x8C
        , 0x8B
        , 0x82
        , 0x85
        , 0xA8
        , 0xAF
        , 0xA6
        , 0xA1
        , 0xB4
        , 0xB3
        , 0xBA
        , 0xBD
        , 0xC7
        , 0xC0
        , 0xC9
        , 0xCE
        , 0xDB
        , 0xDC
        , 0xD5
        , 0xD2
        , 0xFF
        , 0xF8
        , 0xF1
        , 0xF6
        , 0xE3
        , 0xE4
        , 0xED
        , 0xEA
        , 0xB7
        , 0xB0
        , 0xB9
        , 0xBE
        , 0xAB
        , 0xAC
        , 0xA5
        , 0xA2
        , 0x8F
        , 0x88
        , 0x81
        , 0x86
        , 0x93
        , 0x94
        , 0x9D
        , 0x9A
        , 0x27
        , 0x20
        , 0x29
        , 0x2E
        , 0x3B
        , 0x3C
        , 0x35
        , 0x32
        , 0x1F
        , 0x18
        , 0x11
        , 0x16
        , 0x03
        , 0x04
        , 0x0D
        , 0x0A
        , 0x57
        , 0x50
        , 0x59
        , 0x5E
        , 0x4B
        , 0x4C
        , 0x45
        , 0x42
        , 0x6F
        , 0x68
        , 0x61
        , 0x66
        , 0x73
        , 0x74
        , 0x7D
        , 0x7A
        , 0x89
        , 0x8E
        , 0x87
        , 0x80
        , 0x95
        , 0x92
        , 0x9B
        , 0x9C
        , 0xB1
        , 0xB6
        , 0xBF
        , 0xB8
        , 0xAD
        , 0xAA
        , 0xA3
        , 0xA4
        , 0xF9
        , 0xFE
        , 0xF7
        , 0xF0
        , 0xE5
        , 0xE2
        , 0xEB
        , 0xEC
        , 0xC1
        , 0xC6
        , 0xCF
        , 0xC8
        , 0xDD
        , 0xDA
        , 0xD3
        , 0xD4
        , 0x69
        , 0x6E
        , 0x67
        , 0x60
        , 0x75
        , 0x72
        , 0x7B
        , 0x7C
        , 0x51
        , 0x56
        , 0x5F
        , 0x58
        , 0x4D
        , 0x4A
        , 0x43
        , 0x44
        , 0x19
        , 0x1E
        , 0x17
        , 0x10
        , 0x05
        , 0x02
        , 0x0B
        , 0x0C
        , 0x21
        , 0x26
        , 0x2F
        , 0x28
        , 0x3D
        , 0x3A
        , 0x33
        , 0x34
        , 0x4E
        , 0x49
        , 0x40
        , 0x47
        , 0x52
        , 0x55
        , 0x5C
        , 0x5B
        , 0x76
        , 0x71
        , 0x78
        , 0x7F
        , 0x6A
        , 0x6D
        , 0x64
        , 0x63
        , 0x3E
        , 0x39
        , 0x30
        , 0x37
        , 0x22
        , 0x25
        , 0x2C
        , 0x2B
        , 0x06
        , 0x01
        , 0x08
        , 0x0F
        , 0x1A
        , 0x1D
        , 0x14
        , 0x13
        , 0xAE
        , 0xA9
        , 0xA0
        , 0xA7
        , 0xB2
        , 0xB5
        , 0xBC
        , 0xBB
        , 0x96
        , 0x91
        , 0x98
        , 0x9F
        , 0x8A
        , 0x8D
        , 0x84
        , 0x83
        , 0xDE
        , 0xD9
        , 0xD0
        , 0xD7
        , 0xC2
        , 0xC5
        , 0xCC
        , 0xCB
        , 0xE6
        , 0xE1
        , 0xE8
        , 0xEF
        , 0xFA
        , 0xFD
        , 0xF4
        , 0xF3
        ]
