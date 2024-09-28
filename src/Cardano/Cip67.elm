module Cardano.Cip67 exposing
    ( Cip67, AssetName
    , fromBytes, labelFromHex, fromCbor
    , toBytes, labelToHex, toCbor
    )

{-| CIP-0067 support.

CIP-0067 [describes](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0067)
a standard format for assets so that their purpose can be deduced solely by
their token names. So far, the primary application of this standard is to
easily distinguish between CIP-0068 assets and their reference counterparts.

@docs Cip67, AssetName

@docs fromBytes, labelFromHex, fromCbor

@docs toBytes, labelToHex, toCbor

-}

import Bytes
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Crc8 as Crc8
import Bytes.Decode as BD
import Bytes.Encode as BE
import Cardano.MultiAsset as MultiAsset
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
        Maybe.andThen Cip67.fromBytes <| Bytes.fromHex "000de14042756432393231"

    -- Just { assetName = Bytes "42756432393231", label = 222 }

-}
type alias Cip67 =
    { label : Int
    , assetName : Bytes AssetName
    }


{-| Phantom type for CIP-0067 asset names with the label prefix removed.
-}
type AssetName
    = AssetName Never


{-| Validate and separate the label of a CIP-0067 asset name.

Given a valid CIP-0067 token name [Bytes], this function separates the label as
an [Int], and returns the asset name without the label bytes.

-}
fromBytes : Bytes MultiAsset.AssetName -> Maybe Cip67
fromBytes tnBytes =
    let
        tnString =
            Bytes.toHex tnBytes
    in
    labelFromHex (String.left 8 tnString)
        |> Maybe.map
            (\label ->
                { label = label
                , assetName = Bytes.fromHexUnchecked <| String.dropLeft 8 tnString
                }
            )


{-| Extract the label if it is valid. Return Nothing otherwise.
-}
labelFromHex : String -> Maybe Int
labelFromHex fullLabelStr =
    let
        hasBracketsAndProperLength =
            (String.length fullLabelStr == 8)
                && String.startsWith "0" fullLabelStr
                && String.endsWith "0" fullLabelStr
    in
    if hasBracketsAndProperLength then
        let
            labelWithChecksum =
                String.slice 1 -1 fullLabelStr

            labelBytes =
                Bytes.fromHexUnchecked <| String.left 4 labelWithChecksum

            checksumBytes =
                Bytes.fromHexUnchecked <| String.right 2 labelWithChecksum
        in
        if Crc8.digest labelBytes == checksumBytes then
            BD.decode (BD.unsignedInt16 Bytes.BE) (Bytes.toBytes labelBytes)

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
toBytes : Cip67 -> Bytes MultiAsset.AssetName
toBytes { label, assetName } =
    (labelToHex label ++ Bytes.toHex assetName)
        |> Bytes.fromHexUnchecked


{-| Convert an Int label into its CIP-0067 hex string.
-}
labelToHex : Int -> String
labelToHex label =
    let
        labelBytes =
            BE.encode (BE.unsignedInt16 Bytes.BE label)
                |> Bytes.fromBytes

        checksumStr =
            Bytes.toHex <| Crc8.digest labelBytes
    in
    "0" ++ Bytes.toHex labelBytes ++ checksumStr ++ "0"


{-| CBOR encoder.
-}
toCbor : Cip67 -> E.Encoder
toCbor =
    toBytes >> Bytes.toCbor
