module ElmCardano.Redeemer exposing (..)

import Cbor.Encode as E
import ElmCardano.Data as Data exposing (Data)


type alias Redeemer =
    { tag : RedeemerTag -- 0
    , index : Int -- 1
    , data : Data -- 2
    , exUnits : ExUnits -- 3
    }


type RedeemerTag
    = Spend
    | Mint
    | Cert
    | Reward


type alias ExUnits =
    { mem : Int -- 0
    , steps : Int -- 1
    }


encodeRedeemer : Redeemer -> E.Encoder
encodeRedeemer =
    E.tuple <|
        E.elems
            >> E.elem encodeRedeemerTag .tag
            >> E.elem E.int .index
            >> E.elem Data.encode .data
            >> E.elem encodeExUnits .exUnits


encodeRedeemerTag : RedeemerTag -> E.Encoder
encodeRedeemerTag redeemerTag =
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


encodeExUnits : ExUnits -> E.Encoder
encodeExUnits =
    E.tuple <|
        E.elems
            >> E.elem E.int .mem
            >> E.elem E.int .steps
