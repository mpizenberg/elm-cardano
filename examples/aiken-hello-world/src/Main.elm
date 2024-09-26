port module Main exposing (..)

import Browser
import Cardano.Cip30 as Cip30
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Json.Decode as JDecode exposing (Value)


main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
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
    { availableWallets : List Cip30.WalletDescriptor
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { availableWallets = [] }
    , toWallet <| Cip30.encodeRequest Cip30.discoverWallets
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WalletMsg value ->
            case JDecode.decodeValue Cip30.responseDecoder value of
                Ok (Cip30.AvailableWallets wallets) ->
                    ( { model | availableWallets = wallets }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Hello Cardano!" ]
        , div [] [ text "CIP-30 wallets detected:" ]
        , viewAvailableWallets model.availableWallets
        ]


viewAvailableWallets : List Cip30.WalletDescriptor -> Html Msg
viewAvailableWallets wallets =
    let
        walletDescription : Cip30.WalletDescriptor -> String
        walletDescription w =
            "id: " ++ w.id ++ ", name: " ++ w.name

        walletIcon : Cip30.WalletDescriptor -> Html Msg
        walletIcon { icon } =
            Html.img [ src icon, height 32 ] []
    in
    wallets
        |> List.map (\w -> div [] [ walletIcon w, text (walletDescription w) ])
        |> div []
