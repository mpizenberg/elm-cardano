port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
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
    | DiscoverButtonClicked
    | ConnectButtonClicked { id : String, extensions : List Int }
    | GetNetworkIdButtonClicked Wallet.Cip30Wallet
    | GetUtxosButtonClicked Wallet.Cip30Wallet
    | GetBalanceButtonClicked Wallet.Cip30Wallet



-- MODEL


type alias Model =
    { availableWallets : List Wallet.Cip30WalletDescriptor
    , connectedWallets : Dict String Wallet.Cip30Wallet
    , lastApiResponse : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { availableWallets = [], connectedWallets = Dict.empty, lastApiResponse = "" }
    , toWallet <| Wallet.encodeCip30Request Wallet.discoverCip30Wallets
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WalletMsg value ->
            case JDecode.decodeValue Wallet.cip30ResponseDecoder value of
                Ok (Wallet.AvailableCip30Wallets wallets) ->
                    ( { model | availableWallets = wallets }
                    , Cmd.none
                    )

                Ok (Wallet.EnabledCip30Wallet wallet) ->
                    ( addEnabledWallet wallet model
                    , Cmd.none
                    )

                Ok (Wallet.NetworkId { walletId, networkId }) ->
                    ( { model | lastApiResponse = "wallet: " ++ walletId ++ ", network id: " ++ String.fromInt networkId }
                    , Cmd.none
                    )

                Ok (Wallet.WalletUtxos { walletId, utxos }) ->
                    let
                        utxosStr =
                            String.join "\n" utxos
                    in
                    ( { model | lastApiResponse = "wallet: " ++ walletId ++ ", utxos:\n" ++ utxosStr }
                    , Cmd.none
                    )

                Ok (Wallet.WalletBalance { walletId, balance }) ->
                    ( { model | lastApiResponse = "wallet: " ++ walletId ++ ", balance:\n" ++ Debug.toString balance }
                    , Cmd.none
                    )

                Err error ->
                    let
                        _ =
                            JDecode.errorToString error
                                |> Debug.log "Decoding error:"
                    in
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DiscoverButtonClicked ->
            ( model, toWallet <| Wallet.encodeCip30Request Wallet.discoverCip30Wallets )

        ConnectButtonClicked { id, extensions } ->
            ( model, toWallet (Wallet.encodeCip30Request (Wallet.enableCip30Wallet { id = id, extensions = extensions })) )

        GetNetworkIdButtonClicked wallet ->
            ( model, toWallet (Wallet.encodeCip30Request (Wallet.getNetworkId wallet)) )

        GetUtxosButtonClicked wallet ->
            -- Gero does not paginate
            -- Flint does not paginate
            -- NuFi does not paginate
            ( model, toWallet <| Wallet.encodeCip30Request <| Wallet.getUtxos wallet { amount = Nothing, paginate = Just { page = 0, limit = 2 } } )

        GetBalanceButtonClicked wallet ->
            -- Eternl has a weird response
            ( model, toWallet (Wallet.encodeCip30Request (Wallet.getBalance wallet)) )


addEnabledWallet : Wallet.Cip30Wallet -> Model -> Model
addEnabledWallet wallet { availableWallets, connectedWallets } =
    -- Modify the available wallets with the potentially new "enabled" status
    let
        { id, isEnabled } =
            Wallet.cip30WalletDescriptor wallet

        updatedAvailableWallets : List Wallet.Cip30WalletDescriptor
        updatedAvailableWallets =
            availableWallets
                |> List.map
                    (\w ->
                        if w.id == id then
                            { w | isEnabled = isEnabled }

                        else
                            w
                    )
    in
    { availableWallets = updatedAvailableWallets
    , connectedWallets = Dict.insert id wallet connectedWallets
    , lastApiResponse = ""
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Hello Cardano!" ]
        , div [] [ Html.button [ onClick DiscoverButtonClicked ] [ text "discover wallets" ] ]
        , div [] [ text "Available wallets:" ]
        , viewAvailableWallets model.availableWallets
        , div [] [ text "Connected wallets:" ]
        , viewConnectedWallets model.connectedWallets
        , div [] [ text "Last API request response:" ]
        , Html.pre [] [ text model.lastApiResponse ]
        ]


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

        walletIcon : Wallet.Cip30WalletDescriptor -> Html Msg
        walletIcon { icon } =
            Html.img [ src icon, height 32 ] []

        enableButton : Wallet.Cip30WalletDescriptor -> Html Msg
        enableButton { id, supportedExtensions } =
            Html.button [ onClick (ConnectButtonClicked { id = id, extensions = supportedExtensions }) ] [ text "connect" ]
    in
    wallets
        |> List.map (\w -> div [] [ walletIcon w, text (walletDescription w), enableButton w ])
        |> div []


viewConnectedWallets : Dict String Wallet.Cip30Wallet -> Html Msg
viewConnectedWallets wallets =
    let
        walletDescription : Wallet.Cip30WalletDescriptor -> String
        walletDescription w =
            "id: "
                ++ w.id
                ++ ", name: "
                ++ w.name

        walletIcon : Wallet.Cip30WalletDescriptor -> Html Msg
        walletIcon { icon } =
            Html.img [ src icon, height 32 ] []
    in
    Dict.values wallets
        |> List.map (\w -> ( Wallet.cip30WalletDescriptor w, w ))
        |> List.map (\( d, w ) -> div [] (walletIcon d :: text (walletDescription d) :: walletActions w))
        |> div []


walletActions : Wallet.Cip30Wallet -> List (Html Msg)
walletActions wallet =
    [ Html.button [ onClick <| GetNetworkIdButtonClicked wallet ] [ text "getNetworkId" ]
    , Html.button [ onClick <| GetUtxosButtonClicked wallet ] [ text "getUtxos" ]
    , Html.button [ onClick <| GetBalanceButtonClicked wallet ] [ text "getBalance" ]
    ]
