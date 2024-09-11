module Cardano.Redeemer exposing
    ( Redeemer, RedeemerTag(..), ExUnits
    , encodeTag, encodeExUnits
    , tagFromCbor, exUnitsFromCbor
    , encodeAsArray, fromCborArray
    )

{-| Redeemer

@docs Redeemer, RedeemerTag, ExUnits
@docs encode, encodeTag, encodeExUnits
@docs fromCbor, tagFromCbor, exUnitsFromCbor

-}

import Cardano.Data as Data exposing (Data)
import Cbor.Decode as D
import Cbor.Encode as E


{-| Redeemer of a script, containing the data passed as argument to the validator.
-}
type alias Redeemer =
    { tag : RedeemerTag -- 0
    , index : Int -- 1
    , data : Data -- 2
    , exUnits : ExUnits -- 3
    }


{-| Indicator of the type of validator associated with this redeemer.
-}
type RedeemerTag
    = Spend -- 0
    | Mint -- 1
    | Cert -- 2
    | Reward -- 3
    | Vote -- 4
    | Propose -- 5


{-| Cost of the script in memory and instruction steps.
-}
type alias ExUnits =
    { mem : Int -- 0
    , steps : Int -- 1
    }


{-| CBOR decoder for [Redeemer].
-}
fromCborArray : D.Decoder Redeemer
fromCborArray =
    D.oneOf
        [ D.tuple Redeemer <|
            D.elems
                >> D.elem tagFromCbor
                >> D.elem D.int
                >> D.elem Data.fromCbor
                >> D.elem exUnitsFromCbor
        ]


{-| CBOR decoder for [Tag].
-}
tagFromCbor : D.Decoder RedeemerTag
tagFromCbor =
    D.int
        |> D.andThen
            (\tag ->
                case tag of
                    0 ->
                        D.succeed Spend

                    1 ->
                        D.succeed Mint

                    2 ->
                        D.succeed Cert

                    3 ->
                        D.succeed Reward

                    4 ->
                        D.succeed Vote

                    5 ->
                        D.succeed Propose

                    _ ->
                        D.fail
            )


{-| CBOR encoder for a [Redeemer].
-}
encodeAsArray : Redeemer -> E.Encoder
encodeAsArray =
    E.tuple <|
        E.elems
            >> E.elem encodeTag .tag
            >> E.elem E.int .index
            >> E.elem Data.toCbor .data
            >> E.elem encodeExUnits .exUnits


{-| CBOR encoder for a [RedeemerTag].
-}
encodeTag : RedeemerTag -> E.Encoder
encodeTag redeemerTag =
    E.int <|
        case redeemerTag of
            Spend ->
                0

            Mint ->
                1

            Cert ->
                2

            Reward ->
                3

            Vote ->
                4

            Propose ->
                5


{-| CBOR encoder for [ExUnits].
-}
encodeExUnits : ExUnits -> E.Encoder
encodeExUnits =
    E.tuple <|
        E.elems
            >> E.elem E.int .mem
            >> E.elem E.int .steps


{-| CBOR decoder for [ExUnits].
-}
exUnitsFromCbor : D.Decoder ExUnits
exUnitsFromCbor =
    D.tuple ExUnits <|
        D.elems
            >> D.elem D.int
            >> D.elem D.int
