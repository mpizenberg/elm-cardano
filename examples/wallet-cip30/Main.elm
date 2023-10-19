port module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Json.Decode as JDecode exposing (Value, value)
import Wallet


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> fromWallet WalletMsg
        , view = view
        }


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


type Msg
    = WalletMsg Value



-- MODEL


type alias Model =
    { availableWallets : List Wallet.Cip30WalletDescriptor }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { availableWallets = [] }
    , toWallet <| Wallet.encodeCip30Request Wallet.discoverCip30Wallets
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        WalletMsg value ->
            case JDecode.decodeValue Wallet.cip30ResponseDecoder value of
                Ok (Wallet.AvailableCip30Wallets wallets) ->
                    ( { availableWallets = wallets }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ div [] [ text "Hello Cardano!" ], viewAvailableWallets model.availableWallets ]


viewAvailableWallets : List Wallet.Cip30WalletDescriptor -> Html Msg
viewAvailableWallets wallets =
    let
        walletDescription : Wallet.Cip30WalletDescriptor -> String
        walletDescription w =
            "id: "
                ++ w.id
                ++ ", name: "
                ++ w.name
                ++ ", apiVersion: "
                ++ w.apiVersion
                ++ ", isEnabled: "
                ++ Debug.toString w.isEnabled
                ++ ", supportedExtensions: "
                ++ Debug.toString w.supportedExtensions
    in
    wallets
        |> List.map (\w -> div [] [ text (walletDescription w) ])
        |> div []
