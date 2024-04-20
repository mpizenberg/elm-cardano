port module Main exposing (main)

import AppUrl
import Browser
import Bytes.Comparable as Bytes
import Bytes.Encode
import Cardano.Cip30 as Cip30
import Cardano.Value as ECValue
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, src)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as JDecode exposing (Value, value)
import Natural as N
import Url


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fromWallet WalletMsg
        , onUrlChange (locationHrefToRoute >> UrlChanged)
        ]


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg


type Msg
    = UrlChanged Route
    | WalletMsg Value
    | DiscoverButtonClicked
    | ConnectButtonClicked { id : String, extensions : List Int }
    | GetExtensionsButtonClicked Cip30.Wallet
    | GetNetworkIdButtonClicked Cip30.Wallet
    | GetUtxosPaginateButtonClicked Cip30.Wallet
    | GetUtxosAmountButtonClicked Cip30.Wallet
    | GetCollateralButtonClicked Cip30.Wallet
    | GetBalanceButtonClicked Cip30.Wallet
    | GetUsedAddressesButtonClicked Cip30.Wallet
    | GetUnusedAddressesButtonClicked Cip30.Wallet
    | GetChangeAddressButtonClicked Cip30.Wallet
    | GetRewardAddressesButtonClicked Cip30.Wallet
    | SignDataButtonClicked Cip30.Wallet


type Route
    = RouteHome
    | RouteClaim { key1 : Maybe String, key2 : Maybe String }
    | Route404



-- MODEL


type alias Model =
    { route : Route
    , availableWallets : List Cip30.WalletDescriptor
    , connectedWallets : Dict String Cip30.Wallet
    , rewardAddress : Maybe { walletId : String, address : String }
    , lastApiResponse : String
    , lastError : String
    }


init : String -> ( Model, Cmd Msg )
init locationHref =
    ( { route = locationHrefToRoute locationHref
      , availableWallets = []
      , connectedWallets = Dict.empty
      , rewardAddress = Nothing
      , lastApiResponse = ""
      , lastError = ""
      }
    , toWallet <| Cip30.encodeRequest Cip30.discoverWallets
    )



-- NAVIGATION


link : msg -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link href attrs children =
    Html.a (preventDefaultOn "click" (JDecode.succeed ( href, True )) :: attrs) children


locationHrefToRoute : String -> Route
locationHrefToRoute locationHref =
    case Url.fromString locationHref |> Maybe.map AppUrl.fromUrl of
        Nothing ->
            Route404

        Just { path, queryParameters, fragment } ->
            case path of
                [] ->
                    RouteHome

                [ "claim" ] ->
                    RouteClaim
                        { key1 = Dict.get "key1" queryParameters |> Maybe.andThen List.head
                        , key2 = Dict.get "key2" queryParameters |> Maybe.andThen List.head
                        }

                _ ->
                    Route404



-- myParser : Url.Parser (Route -> Route) Route
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged route ->
            ( { model | route = route }, Cmd.none )

        WalletMsg value ->
            case JDecode.decodeValue Cip30.responseDecoder value of
                Ok (Cip30.AvailableWallets wallets) ->
                    ( { model | availableWallets = wallets, lastError = "" }
                    , Cmd.none
                    )

                Ok (Cip30.EnabledWallet wallet) ->
                    ( addEnabledWallet wallet model
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.Extensions extensions)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", extensions: [" ++ String.join ", " (List.map String.fromInt extensions) ++ "]"
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.NetworkId networkId)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", network id: " ++ String.fromInt networkId
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.WalletUtxos utxos)) ->
                    let
                        utxosStr =
                            case utxos of
                                Nothing ->
                                    "undefined"

                                Just utxosList ->
                                    List.map Debug.toString utxosList
                                        |> String.join "\n"
                    in
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", utxos:\n" ++ utxosStr
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.Collateral collateral)) ->
                    let
                        utxosStr =
                            case collateral of
                                Nothing ->
                                    "undefined"

                                Just utxos ->
                                    List.map Debug.toString utxos
                                        |> String.join "\n"
                    in
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", collateral:\n" ++ utxosStr
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.WalletBalance balance)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", balance:\n" ++ Debug.toString balance
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.UsedAddresses usedAddresses)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", used addresses:\n" ++ String.join "\n" usedAddresses
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.UnusedAddresses unusedAddresses)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", unused addresses:\n" ++ String.join "\n" unusedAddresses
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.ChangeAddress changeAddress)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", change address:\n" ++ changeAddress
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.RewardAddresses rewardAddresses)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", reward addresses:\n" ++ String.join "\n" rewardAddresses
                        , rewardAddress = List.head rewardAddresses |> Maybe.map (\addr -> { walletId = walletId, address = addr })
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.ApiResponse { walletId } (Cip30.SignedData signedData)) ->
                    ( { model
                        | lastApiResponse = "wallet: " ++ walletId ++ ", signed data:\n" ++ Debug.toString signedData
                        , lastError = ""
                      }
                    , Cmd.none
                    )

                Ok (Cip30.Error error) ->
                    ( { model | lastError = error }, Cmd.none )

                Ok (Cip30.UnhandledResponseType error) ->
                    ( { model | lastError = error }, Cmd.none )

                Err error ->
                    ( { model | lastError = JDecode.errorToString error }, Cmd.none )

        DiscoverButtonClicked ->
            ( model, toWallet <| Cip30.encodeRequest Cip30.discoverWallets )

        ConnectButtonClicked { id, extensions } ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = extensions })) )

        GetExtensionsButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getExtensions wallet)) )

        GetNetworkIdButtonClicked wallet ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.getNetworkId wallet)) )

        GetUtxosPaginateButtonClicked wallet ->
            -- Gero does not paginate
            -- Flint does not paginate
            -- NuFi does not paginate
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Nothing, paginate = Just { page = 0, limit = 2 } } )

        GetUtxosAmountButtonClicked wallet ->
            -- Lace picks at random (fun!)
            -- Gero does not handle the amount parameter
            -- NuFi does not handle the amount parameter
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Just (ECValue.onlyLovelace <| N.fromSafeInt 14000000), paginate = Nothing } )

        GetCollateralButtonClicked wallet ->
            -- Typhon crashes with the amounts
            -- Nami crashes as the method does not exist
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getCollateral wallet { amount = ECValue.onlyLovelace <| N.fromSafeInt 3000000 } )

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
                                    , payload = Bytes.fromBytes <| Bytes.Encode.encode (Bytes.Encode.unsignedInt8 42)
                                    }
                        )


addEnabledWallet : Cip30.Wallet -> Model -> Model
addEnabledWallet wallet { route, availableWallets, connectedWallets, rewardAddress } =
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
    { route = route
    , availableWallets = updatedAvailableWallets
    , connectedWallets = Dict.insert id wallet connectedWallets
    , rewardAddress = rewardAddress
    , lastApiResponse = ""
    , lastError = ""
    }



-- VIEW


view : Model -> Html Msg
view model =
    case model.route of
        RouteHome ->
            viewHome model

        RouteClaim { key1, key2 } ->
            viewClaim key1 key2 model

        Route404 ->
            view404 model


viewHome : Model -> Html Msg
viewHome model =
    div []
        [ div [] [ text "Hello Cardano!" ]
        , div [] [ Html.button [ onClick DiscoverButtonClicked ] [ text "discover wallets" ] ]
        , div [] [ text "Available wallets:" ]
        , viewAvailableWallets model.availableWallets
        , div [] [ text "Connected wallets:" ]
        , viewConnectedWallets model.connectedWallets
        , div [] [ text "Last API request response:" ]
        , Html.pre [] [ text model.lastApiResponse ]
        , div [] [ text "Last error:" ]
        , Html.pre [] [ text model.lastError ]
        ]


viewClaim : Maybe String -> Maybe String -> Model -> Html Msg
viewClaim key1 key2 _ =
    div []
        [ div [] [ text <| "key1: " ++ Maybe.withDefault "" key1 ]
        , div [] [ text <| "key2: " ++ Maybe.withDefault "" key2 ]
        ]


view404 : Model -> Html Msg
view404 _ =
    div [] [ text "Page not found (error 404)" ]


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
    , Html.button [ onClick <| GetUtxosPaginateButtonClicked wallet ] [ text "getUtxos(paginate:2)" ]
    , Html.button [ onClick <| GetUtxosAmountButtonClicked wallet ] [ text "getUtxos(amount:14ada)" ]
    , Html.button [ onClick <| GetCollateralButtonClicked wallet ] [ text "getCollateral(amount:3ada)" ]
    , Html.button [ onClick <| GetBalanceButtonClicked wallet ] [ text "getBalance" ]
    , Html.button [ onClick <| GetUsedAddressesButtonClicked wallet ] [ text "getUsedAddresses" ]
    , Html.button [ onClick <| GetUnusedAddressesButtonClicked wallet ] [ text "getUnusedAddresses" ]
    , Html.button [ onClick <| GetChangeAddressButtonClicked wallet ] [ text "getChangeAddress" ]
    , Html.button [ onClick <| GetRewardAddressesButtonClicked wallet ] [ text "getRewardAddresses" ]
    , Html.button [ onClick <| SignDataButtonClicked wallet ] [ text "signData" ]
    ]
