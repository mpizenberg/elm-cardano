port module Main exposing (..)

import Browser
import Bytes.Encode
import Cip30
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick)
import Json.Decode as JDecode exposing (Value, value)


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
    | GetExtensionsButtonClicked Cip30.Wallet
    | GetNetworkIdButtonClicked Cip30.Wallet
    | GetUtxosButtonClicked Cip30.Wallet
    | GetBalanceButtonClicked Cip30.Wallet
    | GetUsedAddressesButtonClicked Cip30.Wallet
    | GetUnusedAddressesButtonClicked Cip30.Wallet
    | GetChangeAddressButtonClicked Cip30.Wallet
    | GetRewardAddressesButtonClicked Cip30.Wallet
    | SignDataButtonClicked Cip30.Wallet



-- MODEL


type alias Model =
    { availableWallets : List Cip30.WalletDescriptor
    , connectedWallets : Dict String Cip30.Wallet
    , rewardAddress : Maybe { walletId : String, address : String }
    , lastApiResponse : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { availableWallets = [], connectedWallets = Dict.empty, rewardAddress = Nothing, lastApiResponse = "" }
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

                Ok (Cip30.EnablingError _ _) ->
                    Debug.todo "Handle enable() errors"

                Ok (Cip30.EnabledWallet wallet) ->
                    ( addEnabledWallet wallet model
                    , Cmd.none
                    )

                Ok (Cip30.Extensions { walletId, extensions }) ->
                    ( { model | lastApiResponse = "wallet: " ++ walletId ++ ", extensions: [" ++ String.join ", " (List.map String.fromInt extensions) ++ "]" }
                    , Cmd.none
                    )

                Ok (Cip30.NetworkId { walletId, networkId }) ->
                    ( { model | lastApiResponse = "wallet: " ++ walletId ++ ", network id: " ++ String.fromInt networkId }
                    , Cmd.none
                    )

                Ok (Cip30.WalletUtxos { walletId, utxos }) ->
                    let
                        utxosStr =
                            String.join "\n" utxos
                    in
                    ( { model | lastApiResponse = "wallet: " ++ walletId ++ ", utxos:\n" ++ utxosStr }
                    , Cmd.none
                    )

                Ok (Cip30.WalletBalance { walletId, balance }) ->
                    ( { model | lastApiResponse = "wallet: " ++ walletId ++ ", balance:\n" ++ Debug.toString balance }
                    , Cmd.none
                    )

                Ok (Cip30.UsedAddresses { walletId, usedAddresses }) ->
                    ( { model | lastApiResponse = "wallet: " ++ walletId ++ ", used addresses:\n" ++ String.join "\n" usedAddresses }
                    , Cmd.none
                    )

                Ok (Cip30.UnusedAddresses { walletId, unusedAddresses }) ->
                    ( { model | lastApiResponse = "wallet: " ++ walletId ++ ", unused addresses:\n" ++ String.join "\n" unusedAddresses }
                    , Cmd.none
                    )

                Ok (Cip30.ChangeAddress { walletId, changeAddress }) ->
                    ( { model | lastApiResponse = "wallet: " ++ walletId ++ ", change address:\n" ++ changeAddress }
                    , Cmd.none
                    )

                Ok (Cip30.RewardAddresses { walletId, rewardAddresses }) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", reward addresses:\n" ++ String.join "\n" rewardAddresses
                        , rewardAddress = List.head rewardAddresses |> Maybe.map (\addr -> { walletId = walletId, address = addr })
                      }
                    , Cmd.none
                    )

                Ok (Cip30.SignedData { walletId, signedData }) ->
                    ( { model | lastApiResponse = "wallet: " ++ walletId ++ ", signed data:\n" ++ Debug.toString signedData }
                    , Cmd.none
                    )

                Ok (Cip30.UnhandledResponseType _) ->
                    Debug.todo "Handle unhandled response types"

                Err error ->
                    let
                        _ =
                            JDecode.errorToString error
                                |> Debug.log "Decoding error:"
                    in
                    ( model, Cmd.none )

        DiscoverButtonClicked ->
            ( model, toWallet <| Cip30.encodeRequest Cip30.discoverWallets )

        ConnectButtonClicked { id, extensions } ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = extensions })) )

        GetExtensionsButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getExtensions wallet)) )

        GetNetworkIdButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getNetworkId wallet)) )

        GetUtxosButtonClicked wallet ->
            -- Gero does not paginate
            -- Flint does not paginate
            -- NuFi does not paginate
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Nothing, paginate = Just { page = 0, limit = 2 } } )

        GetBalanceButtonClicked wallet ->
            -- Eternl has sometimes? a weird response
            ( model, toWallet (Cip30.encodeRequest (Cip30.getBalance wallet)) )

        GetUsedAddressesButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getUsedAddresses wallet { paginate = Nothing })) )

        GetUnusedAddressesButtonClicked wallet ->
            -- Lace does not return any unused address
            -- Flint returns the same for unused address as used address
            -- Typhon returns the same for unused address and used address
            -- Eternl returns the same
            -- Eternl and Typhon do not return the same addresses while being on the same wallet?
            -- Nami returns no unused address
            ( model, toWallet (Cip30.encodeRequest (Cip30.getUnusedAddresses wallet)) )

        GetChangeAddressButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getChangeAddress wallet)) )

        GetRewardAddressesButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getRewardAddresses wallet)) )

        SignDataButtonClicked wallet ->
            case model.rewardAddress of
                Nothing ->
                    ( { model | lastApiResponse = "Click on getRewardAddresses for this wallet first." }, Cmd.none )

                Just { walletId, address } ->
                    if walletId /= .id (Cip30.walletDescriptor wallet) then
                        ( { model | lastApiResponse = "Click on getRewardAddresses for this wallet first." }, Cmd.none )

                    else
                        ( model
                        , toWallet <|
                            Cip30.encodeRequest <|
                                Cip30.signData wallet
                                    { addr = address
                                    , payload = Bytes.Encode.encode (Bytes.Encode.unsignedInt8 42)
                                    }
                        )


addEnabledWallet : Cip30.Wallet -> Model -> Model
addEnabledWallet wallet { availableWallets, connectedWallets, rewardAddress } =
    -- Modify the available wallets with the potentially new "enabled" status
    let
        { id, isEnabled } =
            Cip30.walletDescriptor wallet

        updatedAvailableWallets : List Cip30.WalletDescriptor
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
    , rewardAddress = rewardAddress
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


viewAvailableWallets : List Cip30.WalletDescriptor -> Html Msg
viewAvailableWallets wallets =
    let
        walletDescription : Cip30.WalletDescriptor -> String
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

        walletIcon : Cip30.WalletDescriptor -> Html Msg
        walletIcon { icon } =
            Html.img [ src icon, height 32 ] []

        enableButton : Cip30.WalletDescriptor -> Html Msg
        enableButton { id, supportedExtensions } =
            Html.button [ onClick (ConnectButtonClicked { id = id, extensions = supportedExtensions }) ] [ text "connect" ]
    in
    wallets
        |> List.map (\w -> div [] [ walletIcon w, text (walletDescription w), enableButton w ])
        |> div []


viewConnectedWallets : Dict String Cip30.Wallet -> Html Msg
viewConnectedWallets wallets =
    let
        walletDescription : Cip30.WalletDescriptor -> String
        walletDescription w =
            "id: "
                ++ w.id
                ++ ", name: "
                ++ w.name

        walletIcon : Cip30.WalletDescriptor -> Html Msg
        walletIcon { icon } =
            Html.img [ src icon, height 32 ] []
    in
    Dict.values wallets
        |> List.map (\w -> ( Cip30.walletDescriptor w, w ))
        |> List.map (\( d, w ) -> div [] (walletIcon d :: text (walletDescription d) :: walletActions w))
        |> div []


walletActions : Cip30.Wallet -> List (Html Msg)
walletActions wallet =
    [ Html.button [ onClick <| GetExtensionsButtonClicked wallet ] [ text "getExtensions" ]
    , Html.button [ onClick <| GetNetworkIdButtonClicked wallet ] [ text "getNetworkId" ]
    , Html.button [ onClick <| GetUtxosButtonClicked wallet ] [ text "getUtxos" ]
    , Html.button [ onClick <| GetBalanceButtonClicked wallet ] [ text "getBalance" ]
    , Html.button [ onClick <| GetUsedAddressesButtonClicked wallet ] [ text "getUsedAddresses" ]
    , Html.button [ onClick <| GetUnusedAddressesButtonClicked wallet ] [ text "getUnusedAddresses" ]
    , Html.button [ onClick <| GetChangeAddressButtonClicked wallet ] [ text "getChangeAddress" ]
    , Html.button [ onClick <| GetRewardAddressesButtonClicked wallet ] [ text "getRewardAddresses" ]
    , Html.button [ onClick <| SignDataButtonClicked wallet ] [ text "signData" ]
    ]
