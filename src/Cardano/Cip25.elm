module Cardano.Cip25 exposing (Cip25)

{-| CIP-0025 support.

CIP-0025 [describes](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0025)
a standard format for metadata of minting transactions so that off-chain tools
can associate a set of information with the tokens originating (or updating) in
that transaction.

@docs Cip25

-}

import Bytes
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Crc8 as Crc8
import Bytes.Decode as BD
import Bytes.Encode as BE
import Cardano.MultiAsset as MultiAsset
import Cardano.Transaction.AuxiliaryData.Metadatum as Metadatum exposing (Metadatum)
import Cbor.Decode as D
import Cbor.Encode as E
import Json.Decode as JD
import Json.Encode as JE


{-| Datatype for modeling CIP-0025.

The standard simply lays out a set of fields, some of which are optional.

-}
type alias Cip25 =
    { name : String
    , image : Image
    , assetName : Bytes AssetName
    }


{-| Datatype to represent optional files specified for an asset.
-}
type alias File =
    { name : String
    , mediaType : ( MimeType, String )
    , src : String -- Must be decoded from either a string, or an array of strings.
    }


fileToJson : File -> JD.Value
fileToJson file =
    JE.object
        [ ( "name", JE.string file.name )
        , ( "mediaType"
          , if String.length file.src > 64 then
                JE.list JE.string (String.toChunksOf 64 file.src)

            else
                JE.string file.src
          )
        , ( "src"
          , JE.string <|
                String.join
                    "/"
                    [ mimeTypeToString <| Tuple.first file.mimeType
                    , Tuple.second file.mimeType
                    ]
          )
        ]


fileJsonDecoder : JD.Decoder File
fileJsonDecoder =
    JD.map3 File
        (JD.field "name" JD.string)
        -- TODO
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
                                JD.fail "Imvalid media type format."
                    )
            )
        )
        (JD.field "src" <| JD.oneOf JD.string (JD.list JD.string))


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


{-| Dedicated datatype for all image MIME media types according
to [IANA registry](https://iana.org/assignments/media-types/media-types.xhtml#image).

Since the `image` field of CIP-0025 is required, and also must be one of the
image types, this datatype leads to a more robust model (too excessive?).

Having this completely decoupled from [MimeType] may not be a great idea.

-}
type Image
    = Aces
    | Apng
    | Avci
    | Avcs
    | Avif
    | Bmp
    | Cgm
    | DicomRle
    | Dpx
    | Emf
    | Example
    | Fits
    | G3fax
    | Gif
    | Heic
    | HeicSequence
    | Heif
    | HeifSequence
    | Hej2k
    | Hsj2
    | Ief
    | J2c
    | Jls
    | Jp2
    | Jpeg
    | Jph
    | Jphc
    | Jpm
    | Jpx
    | Jxr
    | JxrA
    | JxrS
    | Jxs
    | Jxsc
    | Jxsi
    | Jxss
    | Ktx
    | Ktx2
    | Naplps
    | Png
    | Prs_btif
    | Prs_pti
    | PwgRaster
    | SvgXml
    | T38
    | Tiff
    | TiffFx
    | Vnf_adobe_photoshop
    | Vnd_airzip_accelerator_azv
    | Vnd_cns_inf2
    | Vnd_dece_graph
    | Vnd_djvu
    | Vnd_dwg
    | Vnd_dxf
    | Vnd_dvb_subtit
    | Vnd_fastbidshe
    | Vnd_fpx
    | Vnd_fst
    | Vnd_fujixerox_edmicsMmr
    | Vnd_fujixerox_edmicsRlc
    | Vnd_globalgraphics_pgb
    | Vnd_microsoft_icon
    | Vnd_mix
    | Vnd_msModi
    | Vnd_mozilla_apng
    | Vnd_netFpx
    | Vnd_pco_b16
    | Vnd_radiance
    | Vnd_sealed_png
    | Vnd_sealedmedia_softseal_gif
    | Vnd_sealedmedia_softseal_jpg
    | Vnd_svf
    | Vnd_tencent_tap
    | Vnd_valve_source_texture
    | Vnd_wap_wbmp
    | Vnd_xiff
    | Vnd_zbrush_pcx
    | Webp
    | Wmf


imageMimeTypePrefix =
    "image/"


imageToJson : Image -> JE.Value
imageToJson img =
    let
        postfix =
            case img of
                Aces ->
                    "aces"

                Apng ->
                    "apng"

                Avci ->
                    "avci"

                Avcs ->
                    "avcs"

                Avif ->
                    "avif"

                Bmp ->
                    "bmp"

                Cgm ->
                    "cgm"

                DicomRle ->
                    "dicom-rle"

                Dpx ->
                    "dpx"

                Emf ->
                    "emf"

                Example ->
                    "example"

                Fits ->
                    "fits"

                G3fax ->
                    "g3fax"

                Gif ->
                    "gif"

                Heic ->
                    "heic"

                HeicSequence ->
                    "heic-sequence"

                Heif ->
                    "heif"

                HeifSequence ->
                    "heif-sequence"

                Hej2k ->
                    "hej2k"

                Hsj2 ->
                    "hsj2"

                Ief ->
                    "ief"

                J2c ->
                    "j2c"

                Jls ->
                    "jls"

                Jp2 ->
                    "jp2"

                Jpeg ->
                    "jpeg"

                Jph ->
                    "jph"

                Jphc ->
                    "jphc"

                Jpm ->
                    "jpm"

                Jpx ->
                    "jpx"

                Jxr ->
                    "jxr"

                JxrA ->
                    "jxrA"

                JxrS ->
                    "jxrS"

                Jxs ->
                    "jxs"

                Jxsc ->
                    "jxsc"

                Jxsi ->
                    "jxsi"

                Jxss ->
                    "jxss"

                Ktx ->
                    "ktx"

                Ktx2 ->
                    "ktx2"

                Naplps ->
                    "naplps"

                Png ->
                    "png"

                Prs_btif ->
                    "prs.btif"

                Prs_pti ->
                    "prs.pti"

                PwgRaster ->
                    "pwg-raster"

                SvgXml ->
                    "svg+xml"

                T38 ->
                    "t38"

                Tiff ->
                    "tiff"

                TiffFx ->
                    "tiff-fx"

                Vnf_adobe_photoshop ->
                    "vnf.adobe.photoshop"

                Vnd_airzip_accelerator_azv ->
                    "vnd.airzip.accelerator.azv"

                Vnd_cns_inf2 ->
                    "vnd.cns.inf2"

                Vnd_dece_graphic ->
                    "vnd.dece.graphic"

                Vnd_djvu ->
                    "vnd.djvu"

                Vnd_dwg ->
                    "vnd.dwg"

                Vnd_dxf ->
                    "vnd.dxf"

                Vnd_dvb_subtitle ->
                    "vnd.dvb.subtitle"

                Vnd_fastbidsheet ->
                    "vnd.fastbidsheet"

                Vnd_fpx ->
                    "vnd.fpx"

                Vnd_fst ->
                    "vnd.fst"

                Vnd_fujixerox_edmicsMmr ->
                    "vnd.fujixerox.edmics-mmr"

                Vnd_fujixerox_edmicsRlc ->
                    "vnd.fujixerox.edmics-rlc"

                Vnd_globalgraphics_pgb ->
                    "vnd.globalgraphics.pgb"

                Vnd_microsoft_icon ->
                    "vnd.microsoft.icon"

                Vnd_mix ->
                    "vnd.mix"

                Vnd_msModi ->
                    "vnd.ms-modi"

                Vnd_mozilla_apng ->
                    "vnd.mozilla.apng"

                Vnd_netFpx ->
                    "vnd.net-fpx"

                Vnd_pco_b16 ->
                    "vnd.pco.b16"

                Vnd_radiance ->
                    "vnd.radiance"

                Vnd_sealed_png ->
                    "vnd.sealed.png"

                Vnd_sealedmedia_softseal_gif ->
                    "vnd.sealedmedia.softseal.gif"

                Vnd_sealedmedia_softseal_jpg ->
                    "vnd.sealedmedia.softseal.jpg"

                Vnd_svf ->
                    "vnd.svf"

                Vnd_tencent_tap ->
                    "vnd.tencent.tap"

                Vnd_valve_source_texture ->
                    "vnd.valve.source.texture"

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
    JE.string <| imageMimeTypePrefix ++ postfix


imageJsonDecoder : JD.Decoder Image
imageJsonDecoder =
    JD.string
        |> JD.andThen
            (\fullStr ->
                let
                    prefixLength =
                        String.length imageMimeTypePrefix

                    prefixIsValid =
                        String.startsWith imageMimeTypePrefix fullStr
                in
                if prefixIsValid then
                    case String.dropLeft prefixLength fullStr of
                        "aces" ->
                            D.succeed Aces

                        "apng" ->
                            D.succeed Apng

                        "avci" ->
                            D.succeed Avci

                        "avcs" ->
                            D.succeed Avcs

                        "avif" ->
                            D.succeed Avif

                        "bmp" ->
                            D.succeed Bmp

                        "cgm" ->
                            D.succeed Cgm

                        "dicom-rle" ->
                            D.succeed DicomRle

                        "dpx" ->
                            D.succeed Dpx

                        "emf" ->
                            D.succeed Emf

                        "example" ->
                            D.succeed Example

                        "fits" ->
                            D.succeed Fits

                        "g3fax" ->
                            D.succeed G3fax

                        "gif" ->
                            D.succeed Gif

                        "heic" ->
                            D.succeed Heic

                        "heic-sequence" ->
                            D.succeed HeicSequence

                        "heif" ->
                            D.succeed Heif

                        "heif-sequence" ->
                            D.succeed HeifSequence

                        "hej2k" ->
                            D.succeed Hej2k

                        "hsj2" ->
                            D.succeed Hsj2

                        "ief" ->
                            D.succeed Ief

                        "j2c" ->
                            D.succeed J2c

                        "jls" ->
                            D.succeed Jls

                        "jp2" ->
                            D.succeed Jp2

                        "jpeg" ->
                            D.succeed Jpeg

                        "jph" ->
                            D.succeed Jph

                        "jphc" ->
                            D.succeed Jphc

                        "jpm" ->
                            D.succeed Jpm

                        "jpx" ->
                            D.succeed Jpx

                        "jxr" ->
                            D.succeed Jxr

                        "jxrA" ->
                            D.succeed JxrA

                        "jxrS" ->
                            D.succeed JxrS

                        "jxs" ->
                            D.succeed Jxs

                        "jxsc" ->
                            D.succeed Jxsc

                        "jxsi" ->
                            D.succeed Jxsi

                        "jxss" ->
                            D.succeed Jxss

                        "ktx" ->
                            D.succeed Ktx

                        "ktx2" ->
                            D.succeed Ktx2

                        "naplps" ->
                            D.succeed Naplps

                        "png" ->
                            D.succeed Png

                        "prs.btif" ->
                            D.succeed Prs_btif

                        "prs.pti" ->
                            D.succeed Prs_pti

                        "pwg-raster" ->
                            D.succeed PwgRaster

                        "svg+xml" ->
                            D.succeed SvgXml

                        "t38" ->
                            D.succeed T38

                        "tiff" ->
                            D.succeed Tiff

                        "tiff-fx" ->
                            D.succeed TiffFx

                        "vnf.adobe.photoshop" ->
                            D.succeed Vnf_adobe_photoshop

                        "vnd.airzip.accelerator.azv" ->
                            D.succeed Vnd_airzip_accelerator_azv

                        "vnd.cns.inf2" ->
                            D.succeed Vnd_cns_inf2

                        "vnd.dece.graphic" ->
                            D.succeed Vnd_dece_graphic

                        "vnd.djvu" ->
                            D.succeed Vnd_djvu

                        "vnd.dwg" ->
                            D.succeed Vnd_dwg

                        "vnd.dxf" ->
                            D.succeed Vnd_dxf

                        "vnd.dvb.subtitle" ->
                            D.succeed Vnd_dvb_subtitle

                        "vnd.fastbidsheet" ->
                            D.succeed Vnd_fastbidsheet

                        "vnd.fpx" ->
                            D.succeed Vnd_fpx

                        "vnd.fst" ->
                            D.succeed Vnd_fst

                        "vnd.fujixerox.edmics-mmr" ->
                            D.succeed Vnd_fujixerox_edmicsMmr

                        "vnd.fujixerox.edmics-rlc" ->
                            D.succeed Vnd_fujixerox_edmicsRlc

                        "vnd.globalgraphics.pgb" ->
                            D.succeed Vnd_globalgraphics_pgb

                        "vnd.microsoft.icon" ->
                            D.succeed Vnd_microsoft_icon

                        "vnd.mix" ->
                            D.succeed Vnd_mix

                        "vnd.ms-modi" ->
                            D.succeed Vnd_msModi

                        "vnd.mozilla.apng" ->
                            D.succeed Vnd_mozilla_apng

                        "vnd.net-fpx" ->
                            D.succeed Vnd_netFpx

                        "vnd.pco.b16" ->
                            D.succeed Vnd_pco_b16

                        "vnd.radiance" ->
                            D.succeed Vnd_radiance

                        "vnd.sealed.png" ->
                            D.succeed Vnd_sealed_png

                        "vnd.sealedmedia.softseal.gif" ->
                            D.succeed Vnd_sealedmedia_softseal_gif

                        "vnd.sealedmedia.softseal.jpg" ->
                            D.succeed Vnd_sealedmedia_softseal_jpg

                        "vnd.svf" ->
                            D.succeed Vnd_svf

                        "vnd.tencent.tap" ->
                            D.succeed Vnd_tencent_tap

                        "vnd.valve.source.texture" ->
                            D.succeed Vnd_valve_source_texture

                        "vnd.wap.wbmp" ->
                            D.succeed Vnd_wap_wbmp

                        "vnd.xiff" ->
                            D.succeed Vnd_xiff

                        "vnd.zbrush.pcx" ->
                            D.succeed Vnd_zbrush_pcx

                        "webp" ->
                            D.succeed Webp

                        "wmf" ->
                            D.succeed Wmf

                        _ ->
                            D.fail "Unregistered image MIME type encountered."

                else
                    D.fail "Not an image MIME type."
            )
