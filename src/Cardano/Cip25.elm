module Cardano.Cip25 exposing (Cip25)

{-| CIP-0025 support.

CIP-0025 [describes](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0025)
a standard format for metadata of minting transactions so that off-chain tools
can associate a set of information with the tokens originating (or updating) in
that transaction.

@docs Cip25

-}

import Cardano.Transaction.AuxiliaryData.Metadatum as Metadatum exposing (Metadatum)
import Json.Decode as JD
import Json.Decode.Extra as JD
import Json.Encode as JE
import Json.Encode.Extra as JE
import String.Extra as String


{-| Datatype for modeling CIP-0025.

The standard simply lays out a set of fields, some of which are optional.

-}
type alias Cip25 =
    { name : String
    , image : Image
    , mediaType : ImageMime
    , description : String
    , files : List File
    , version : ( Int, Int )
    , otherProps : Dict String Metadatum
    }


{-| Helper datatype for the `image` field of CIP-0025.

The image can be either a URI that points to a resource with MIME type
`image/*`, or an inline base64-encoded string.

Should this datatype also support inlined SVG strings? e.g.
`data:image/svg+xml,%3Csvg width='45' viewBox=... svg%3E`

-}
type Image
    = ImageUri
        { scheme : String
        , cid : String
        }
    | InlineImage
        { mediaType : ImageMime
        , base64Encoded : String
        }


imageToJson : Image -> JE.Value
imageToJson img =
    case img of
        ImageUri scheme cid ->
            JE.longString <| scheme ++ "://" ++ cid

        InlineImage mediaType base64 ->
            JE.longString <|
                "data:"
                    ++ imageMimeToString mediaType
                    ++ ";base64,"
                    ++ base64


{-| Datatype to represent optional files specified for an asset.
-}
type alias File =
    { name : String
    , mediaType : ( MimeType, String )
    , src : String -- Must be decoded from either a string, or an array of strings.

    -- TODO
    -- , otherProps : Dict String Metadatum
    }


fileToJson : File -> JD.Value
fileToJson file =
    JE.object
        [ ( "name", JE.string file.name )
        , ( "mediaType"
          , JE.string <|
                String.join
                    "/"
                    [ Tuple.first file.mimeType |> mimeTypeToString
                    , Tuple.second file.mimeType
                    ]
          )
        , ( "src", JE.longString file.src )
        ]


fileJsonDecoder : JD.Decoder File
fileJsonDecoder =
    JD.map3 File
        (JD.field "name" JD.string)
        (JD.field
            "mediaType"
            (JD.string
                |> JD.andThen
                    (\fullMimeStr ->
                        case String.split "/" fullMimeStr of
                            [ mimeStr, subMimeStr ] ->
                                case mimeTypeFromString mimeStr of
                                    Just mimeType ->
                                        JD.succeed ( mimeType, subMimeStr )

                                    Nothing ->
                                        JD.fail "Invalid media type."

                            _ ->
                                JD.fail "Invalid media type format."
                    )
            )
        )
        (JD.field "src" JD.longString)


{-| Sum type to represent MIME Content Types listed in [IANA registry](https://iana.org/assignments/media-types/media-types.xhtml).
-}
type MimeType
    = ApplicationMimeType
    | AudioMimeType
    | FontMimeType
    | ExampleMimeType
    | ImageMimeType
    | MessageMimeType
    | ModelMimeType
    | MultipartMimeType
    | TextMimeType
    | VideoMimeType


mimeTypeToString : MimeType -> String
mimeTypeToString mimeType =
    case mimeType of
        ApplicationMimeType ->
            "application"

        AudioMimeType ->
            "audio"

        FontMimeType ->
            "font"

        ExampleMimeType ->
            "example"

        ImageMimeType ->
            "image"

        MessageMimeType ->
            "message"

        ModelMimeType ->
            "model"

        MultipartMimeType ->
            "multipart"

        TextMimeType ->
            "text"

        VideoMimeType ->
            "video"


mimeTypeFromString : String -> Maybe MimeType
mimeTypeFromString mimeTypeStr =
    case mimeTypeStr of
        "application" ->
            Just ApplicationMimeType

        "audio" ->
            Just AudioMimeType

        "font" ->
            Just FontMimeType

        "example" ->
            Just ExampleMimeType

        "image" ->
            Just ImageMimeType

        "message" ->
            Just MessageMimeType

        "model" ->
            Just ModelMimeType

        "multipart" ->
            Just MultipartMimeType

        "text" ->
            Just TextMimeType

        "video" ->
            Just VideoMimeType

        _ ->
            Nothing


{-| Dedicated datatype for all image MIME media types according
to [IANA registry](https://iana.org/assignments/media-types/media-types.xhtml#image).

Since the `image` field of CIP-0025 is required, and also must be one of the
image types, this datatype leads to a more robust model (too excessive?).

Having this completely decoupled from [MimeType] may not be a great idea.

-}
type ImageMime
    = Bmp
    | Gif
    | Jpeg
    | Png
    | SvgXml
    | Tiff
    | Vnf_adobe_photoshop
    | Vnd_dwg
    | Vnd_dxf
    | Webp
    | Wmf


imageMimePrefix =
    mimeTypeToString ImageMimeType ++ "/"


imageMimeToString : Image -> String
imageMimeToString img =
    let
        postfix =
            case img of
                Bmp ->
                    "bmp"

                Gif ->
                    "gif"

                Jpeg ->
                    "jpeg"

                Png ->
                    "png"

                SvgXml ->
                    "svg+xml"

                Tiff ->
                    "tiff"

                Vnf_adobe_photoshop ->
                    "vnf.adobe.photoshop"

                Vnd_dwg ->
                    "vnd.dwg"

                Vnd_dxf ->
                    "vnd.dxf"

                Vnd_wap_wbmp ->
                    "vnd.wap.wbmp"

                Vnd_xiff ->
                    "vnd.xiff"

                Vnd_zbrush_pcx ->
                    "vnd.zbrush.pcx"

                Webp ->
                    "webp"

                Wmf ->
                    "wmf"
    in
    imageMimePrefix ++ postfix


imageMimeToJson : Image -> JE.Value
imageMimeToJson =
    JE.string << imageMimeToString


imageMimeJsonDecoder : JD.Decoder ImageMime
imageMimeJsonDecoder =
    JD.string
        |> JD.andThen
            (\fullStr ->
                let
                    prefixLength =
                        String.length imageMimePrefix

                    prefixIsValid =
                        String.startsWith imageMimePrefix fullStr
                in
                if prefixIsValid then
                    case String.dropLeft prefixLength fullStr of
                        "bmp" ->
                            D.succeed Bmp

                        "gif" ->
                            D.succeed Gif

                        "jpeg" ->
                            D.succeed Jpeg

                        "png" ->
                            D.succeed Png

                        "svg+xml" ->
                            D.succeed SvgXml

                        "tiff" ->
                            D.succeed Tiff

                        "vnf.adobe.photoshop" ->
                            D.succeed Vnf_adobe_photoshop

                        "vnd.dwg" ->
                            D.succeed Vnd_dwg

                        "vnd.dxf" ->
                            D.succeed Vnd_dxf

                        "webp" ->
                            D.succeed Webp

                        "wmf" ->
                            D.succeed Wmf

                        _ ->
                            D.fail "Unregistered image MIME subtype encountered."

                else
                    D.fail "Not an image MIME type."
            )
