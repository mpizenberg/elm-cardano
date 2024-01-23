module Cardano.Cip25 exposing
    ( Cip25
    , File, Image, jsonDecoder, toJson
    )

{-| CIP-0025 support.

CIP-0025 [describes](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0025)
a standard format for metadata of minting transactions so that off-chain tools
can associate a set of information with the tokens originating (or updating) in
that transaction.

@docs Cip25

-}

import Cardano.Transaction.AuxiliaryData.Metadatum as Metadatum exposing (Metadatum)
import Dict exposing (Dict)
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
    , mediaType : Maybe ImageMime
    , description : String
    , files : List File
    , version : Version
    , otherProps : Dict String Metadatum
    }


toJson : Cip25 -> JE.Value
toJson cip25 =
    let
        requiredFields =
            ( "name", JE.string cip25.name )
                :: ( "image", imageToJson cip25.image )
                :: ( "version", versionToJson cip25.version )
                :: Dict.foldr
                    (\k v acc -> ( k, Metadatum.toJson v ) :: acc)
                    []
                    cip25.otherProps

        optionalFields =
            [ Maybe.map
                (\imageMime -> ( "mediaType", imageMimeToJson imageMime ))
                cip25.mediaType
            , case cip25.description of
                "" ->
                    Nothing

                _ ->
                    Just ( "description", JE.string cip25.description )
            , case cip25.files of
                [] ->
                    Nothing

                _ ->
                    Just ( "files", JE.list fileToJson cip25.files )
            ]
    in
    JE.object <| requiredFields ++ List.filterMap identity optionalFields


jsonDecoder : JD.Decoder Cip25
jsonDecoder =
    JD.fail "TODO"


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
        ImageUri props ->
            JE.longString <| props.scheme ++ "://" ++ props.cid

        InlineImage props ->
            JE.longString <|
                "data:"
                    ++ imageMimeToString props.mediaType
                    ++ ";base64,"
                    ++ props.base64Encoded


type alias Version =
    { primary : Int
    , secondary : Int
    }


versionToJson : Version -> JE.Value
versionToJson version =
    JE.string <|
        String.join
            "."
            [ String.fromInt version.primary
            , String.fromInt version.secondary
            ]


versionJsonDecoder : JD.Decoder Version
versionJsonDecoder =
    JD.string
        |> JD.andThen
            (\verStr ->
                case List.map String.toInt (String.split "." verStr) of
                    [ Just primaryInt, Just secondaryInt ] ->
                        JD.succeed <| Version primaryInt secondaryInt

                    _ ->
                        JD.fail "Invalid version string."
            )


{-| Datatype to represent optional files specified for an asset.
-}
type alias File =
    { name : String
    , mediaType : ( MimeType, String )
    , src : String -- Must be decoded from either a string, or an array of strings.
    , otherProps : Dict String Metadatum
    }


fileToJson : File -> JD.Value
fileToJson file =
    JE.object <|
        ( "name", JE.string file.name )
            :: ( "mediaType"
               , JE.string <|
                    String.join
                        "/"
                        [ Tuple.first file.mediaType |> mimeTypeToString
                        , Tuple.second file.mediaType
                        ]
               )
            :: ( "src", JE.longString file.src )
            :: Dict.foldr
                (\k v acc -> ( k, Metadatum.toJson v ) :: acc)
                []
                file.otherProps


fileJsonDecoder : JD.Decoder File
fileJsonDecoder =
    JD.map4 File
        (JD.field "name" JD.string)
        (JD.field "mediaType" mimeStringDecoder)
        (JD.field "src" JD.longString)
        (JD.dict Metadatum.jsonDecoder)


mimeStringDecoder : JD.Decoder ( MimeType, String )
mimeStringDecoder =
    JD.string
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
image types, this datatype leads to a more robust model with a compromise of
a limited support.

TODO: Adding a custom variant (arbitrary string) will allow
two representations for defined constructors. However seems inevitable if
for future support. This also applies to [MimeType]'s current implementation.

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


imageMimeToString : ImageMime -> String
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

                Webp ->
                    "webp"

                Wmf ->
                    "wmf"
    in
    imageMimePrefix ++ postfix


imageMimeToJson : ImageMime -> JE.Value
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
                            JD.succeed Bmp

                        "gif" ->
                            JD.succeed Gif

                        "jpeg" ->
                            JD.succeed Jpeg

                        "png" ->
                            JD.succeed Png

                        "svg+xml" ->
                            JD.succeed SvgXml

                        "tiff" ->
                            JD.succeed Tiff

                        "vnf.adobe.photoshop" ->
                            JD.succeed Vnf_adobe_photoshop

                        "vnd.dwg" ->
                            JD.succeed Vnd_dwg

                        "vnd.dxf" ->
                            JD.succeed Vnd_dxf

                        "webp" ->
                            JD.succeed Webp

                        "wmf" ->
                            JD.succeed Wmf

                        _ ->
                            JD.fail "Unregistered image MIME subtype encountered."

                else
                    JD.fail "Not an image MIME type."
            )
