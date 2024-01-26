module Bytes.Fixed exposing
    ( Bytes16
    , Bytes28
    , Bytes32
    , Bytes4
    , Bytes64
    , bytes16
    , bytes28
    , bytes32
    , bytes4
    , bytes64
    )

import Bytes as RawBytes
import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Bytes.Decode as BD
import Cbor.Decode as CD
import Cbor.Decode.Extra as CD
import Cbor.Encode as CE
import Json.Decode as JD
import Json.Encode as JE


type Bytes4
    = Bytes4 (Bytes Any)


bytes4 : Utilities Bytes4
bytes4 =
    mkUtilitiesFor Bytes4 (\(Bytes4 bs) -> bs) 4


type Bytes16
    = Bytes16 (Bytes Any)


bytes16 : Utilities Bytes16
bytes16 =
    mkUtilitiesFor Bytes16 (\(Bytes16 bs) -> bs) 16


type Bytes28
    = Bytes28 (Bytes Any)


bytes28 : Utilities Bytes28
bytes28 =
    mkUtilitiesFor Bytes28 (\(Bytes28 bs) -> bs) 28


type Bytes32
    = Bytes32 (Bytes Any)


bytes32 : Utilities Bytes32
bytes32 =
    mkUtilitiesFor Bytes32 (\(Bytes32 bs) -> bs) 32


type Bytes64
    = Bytes64 (Bytes Any)


bytes64 : Utilities Bytes64
bytes64 =
    mkUtilitiesFor Bytes64 (\(Bytes64 bs) -> bs) 64



-- Higher Order Helpers


type alias Utilities t =
    { fromBytes : Bytes Any -> Maybe t
    , rawBytesDecoder : BD.Decoder t
    , toBytes : t -> Bytes Any
    , toCbor : t -> CE.Encoder
    , cborDecoder : CD.Decoder t
    , jsonDecoder : JD.Decoder t
    , fromString : String -> Maybe t
    , toString : t -> String
    }


mkUtilitiesFor : (Bytes Any -> t) -> (t -> Bytes Any) -> Int -> Utilities t
mkUtilitiesFor bytesToData dataToBytes bytesCount =
    let
        strLength =
            bytesCount * 2

        fromComparableBytes =
            mkFromBytes bytesCount >> Maybe.map bytesToData
    in
    { fromBytes = fromComparableBytes
    , rawBytesDecoder = BD.bytes bytesCount |> BD.map (bytesToData << Bytes.fromBytes)
    , toBytes = dataToBytes
    , toCbor = dataToBytes >> Bytes.toCbor
    , cborDecoder = CD.fromMaybe <| CD.map (fromComparableBytes << Bytes.fromBytes) CD.bytes
    , jsonDecoder = mkJsonDecoder bytesCount |> JD.map bytesToData
    , fromString = mkFromString strLength >> Maybe.map bytesToData
    , toString = dataToBytes >> Bytes.toString
    }


mkFromBytes : Int -> (Bytes Any -> Maybe (Bytes Any))
mkFromBytes bytesCount =
    \bs ->
        if Bytes.width bs == bytesCount then
            Just bs

        else
            Nothing


mkFromString : Int -> (String -> Maybe (Bytes Any))
mkFromString strLength =
    \hexStr ->
        if String.length hexStr == strLength then
            Bytes.fromString hexStr |> Maybe.map Bytes.toAny

        else
            Nothing


mkJsonDecoder : Int -> JD.Decoder (Bytes Any)
mkJsonDecoder bytesCount =
    JD.string
        |> JD.andThen
            (\str ->
                case mkFromString (bytesCount * 2) str of
                    Just bytes ->
                        JD.succeed bytes

                    Nothing ->
                        JD.fail "Invalid hex string."
            )
