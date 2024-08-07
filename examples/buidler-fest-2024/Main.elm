port module Main exposing (main)

import AppUrl exposing (AppUrl)
import Browser
import Bytes.Comparable as Bytes
import Bytes.Encode
import Cardano.Address as Address exposing (Address)
import Cardano.Cip30 as Cip30
import Cardano.Transaction.Builder as Tx
import Cardano.Utxo as Utxo
import Cardano.Value as CValue
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as HA exposing (height, src)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Json.Decode as JDecode exposing (Value, value)
import Natural as N
import Ogmios6
import Url


main : Program ( String, String ) Model Msg
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
        , fromOgmios OgmiosMsg
        , keysValidityChecked KeysValidityChecked
        ]


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg


port toOgmios : Value -> Cmd msg


port fromOgmios : (Value -> msg) -> Sub msg


port checkKeysValidity : { key1 : String, key2 : String, seed23 : List String, publicAddress : String } -> Cmd msg


port keysValidityChecked :
    ({ key1 : String
     , key2 : String
     , privateKey : String
     , publicAddress : String
     , validity : Bool
     }
     -> msg
    )
    -> Sub msg


type Msg
    = UrlChanged Route
    | OgmiosMsg Value
      -- Wallet stuff
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
      -- Keys stuff
    | KeyInputChange String
    | CheckKeyInput String
    | KeysValidityChecked
        { key1 : String
        , key2 : String
        , privateKey : String
        , publicAddress : String
        , validity : Bool
        }


type Route
    = RouteHome
    | RouteClaim Utxo.OutputReference { key1 : Maybe String, key2 : Maybe String }
    | Route404



-- MODEL


type alias Model =
    { route : Route
    , ogmiosConnection : OgmiosConnectionStatus
    , websocketAddress : String
    , availableWallets : List Cip30.WalletDescriptor
    , connectedWallets : Dict String Cip30.Wallet
    , rewardAddress : Maybe { walletId : String, address : String }
    , lastApiResponse : String
    , lastError : String
    , keyInput : String
    , utxo : UtxoStatus
    , words3To23 : List String
    , keysValidity :
        { key1 : String
        , key2 : String
        , privateKey : String
        , publicAddress : String
        , validity : Bool
        }
    }


type UtxoStatus
    = UnknownUtxoStatus
    | FetchingUtxoAt Utxo.OutputReference
      -- IMPROVE: use actual address type
    | UtxoContents Utxo.OutputReference { address : String, value : CValue.Value }
    | UtxoConsumedAlready


type OgmiosConnectionStatus
    = OgmiosDisconnected
    | OgmiosConnecting
    | OgmiosConnected { websocket : Value, connectionId : String }


init : ( String, String ) -> ( Model, Cmd Msg )
init ( locationHref, websocketAddress ) =
    let
        route =
            locationHrefToRoute locationHref
    in
    ( { route = route
      , ogmiosConnection = OgmiosConnecting
      , websocketAddress = websocketAddress
      , availableWallets = []
      , connectedWallets = Dict.empty
      , rewardAddress = Nothing
      , lastApiResponse = ""
      , lastError = ""
      , keyInput = ""
      , utxo =
            case route of
                RouteClaim outputRef _ ->
                    FetchingUtxoAt outputRef

                _ ->
                    UnknownUtxoStatus
      , words3To23 = [ "zero", "keen", "woman", "gospel", "spend", "pupil", "analyst", "lava", "lens", "private", "dice", "add", "breeze", "joy", "bundle", "junior", "then", "throw", "inside", "stove", "goose" ]
      , keysValidity = { key1 = "", key2 = "", privateKey = "", publicAddress = "", validity = False }
      }
    , Cmd.batch
        [ toWallet <| Cip30.encodeRequest Cip30.discoverWallets
        , connectCmd websocketAddress
        , routePostCmds OgmiosDisconnected route
        ]
    )


routePostCmds : OgmiosConnectionStatus -> Route -> Cmd Msg
routePostCmds connection route =
    case ( route, connection ) of
        ( RouteClaim outputRef _, OgmiosConnected { websocket } ) ->
            -- Check utxo content if exists or consumed
            utxoLookup websocket outputRef

        _ ->
            Cmd.none


utxoLookup : Value -> Utxo.OutputReference -> Cmd Msg
utxoLookup websocket { transactionId, outputIndex } =
    Ogmios6.queryLedgerStateUtxo
        { websocket = websocket
        , txId = Bytes.toString transactionId
        , index = outputIndex
        }
        |> Ogmios6.encodeRequest
        |> toOgmios



-- NAVIGATION


link : msg -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link href attrs children =
    Html.a (preventDefaultOn "click" (JDecode.succeed ( href, True )) :: attrs) children


locationHrefToRoute : String -> Route
locationHrefToRoute locationHref =
    case Url.fromString (Debug.log "loc" locationHref) |> Maybe.map AppUrl.fromUrl of
        Nothing ->
            Route404

        Just { path, queryParameters, fragment } ->
            case path of
                [] ->
                    RouteHome

                [ "claim", txId, index ] ->
                    RouteClaim
                        -- IMPROVE: do not use fromStringUnchecked and withDefault ...
                        { transactionId = Bytes.fromStringUnchecked txId
                        , outputIndex = String.toInt index |> Maybe.withDefault 0
                        }
                        { key1 = Dict.get "key1" queryParameters |> Maybe.andThen List.head
                        , key2 = Dict.get "key2" queryParameters |> Maybe.andThen List.head
                        }

                _ ->
                    Route404


routeToAppUrl : Route -> AppUrl
routeToAppUrl route =
    case route of
        RouteHome ->
            AppUrl.fromPath []

        Route404 ->
            AppUrl.fromPath [ "404" ]

        RouteClaim { transactionId, outputIndex } { key1, key2 } ->
            { path = [ "claim", Bytes.toString transactionId, String.fromInt outputIndex ]
            , queryParameters =
                case ( key1, key2 ) of
                    ( Nothing, Nothing ) ->
                        Dict.empty

                    ( Just k1, Nothing ) ->
                        Dict.singleton "key1" [ k1 ]

                    ( Nothing, Just k2 ) ->
                        Dict.singleton "key2" [ k2 ]

                    ( Just k1, Just k2 ) ->
                        Dict.fromList [ ( "key1", [ k1 ] ), ( "key2", [ k2 ] ) ]
            , fragment = Nothing
            }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged route ->
            ( { model | route = route }
            , routePostCmds model.ogmiosConnection route
            )

        OgmiosMsg value ->
            case JDecode.decodeValue Ogmios6.responseDecoder value of
                Ok (Ogmios6.Connected connection) ->
                    handleConnection connection model

                Ok (Ogmios6.Disconnected _) ->
                    handleDisconnection model

                Ok (Ogmios6.ApiResponse _ response) ->
                    handleApiResponse response { model | lastApiResponse = Debug.toString response }

                Ok (Ogmios6.Error error) ->
                    ( { model | lastError = error }, Cmd.none )

                Ok (Ogmios6.UnhandledResponseType error) ->
                    ( { model | lastError = error }, Cmd.none )

                Err error ->
                    ( { model | lastError = JDecode.errorToString error }, Cmd.none )

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
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getUtxos wallet { amount = Just (CValue.onlyLovelace <| N.fromSafeInt 14000000), paginate = Nothing } )

        GetCollateralButtonClicked wallet ->
            -- Typhon crashes with the amounts
            -- Nami crashes as the method does not exist
            ( model, toWallet <| Cip30.encodeRequest <| Cip30.getCollateral wallet { amount = CValue.onlyLovelace <| N.fromSafeInt 3000000 } )

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

        KeyInputChange keyInput ->
            ( { model | keyInput = keyInput }, Cmd.none )

        CheckKeyInput keyInput ->
            case model.route of
                RouteClaim ref { key1, key2 } ->
                    case ( key1, key2 ) of
                        ( Nothing, Nothing ) ->
                            ( model, pushUrl <| AppUrl.toString <| routeToAppUrl <| RouteClaim ref { key1 = Just keyInput, key2 = key2 } )

                        ( Just k1, Nothing ) ->
                            ( model, pushUrl <| AppUrl.toString <| routeToAppUrl <| RouteClaim ref { key1 = Just k1, key2 = Just keyInput } )

                        ( Nothing, Just k2 ) ->
                            ( model, pushUrl <| AppUrl.toString <| routeToAppUrl <| RouteClaim ref { key1 = Just keyInput, key2 = Just k2 } )

                        ( Just _, Just _ ) ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeysValidityChecked keysValidity ->
            ( { model | keysValidity = keysValidity }, Cmd.none )


addEnabledWallet : Cip30.Wallet -> Model -> Model
addEnabledWallet wallet ({ availableWallets, connectedWallets } as model) =
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
    { model
        | availableWallets = updatedAvailableWallets
        , connectedWallets = Dict.insert id wallet connectedWallets
        , lastApiResponse = ""
        , lastError = ""
    }



-- OGMIOS


handleConnection : { connectionId : String, ws : Value } -> Model -> ( Model, Cmd Msg )
handleConnection { connectionId, ws } model =
    ( { model | ogmiosConnection = OgmiosConnected { websocket = ws, connectionId = connectionId } }
    , case model.route of
        RouteClaim outputRef _ ->
            utxoLookup ws outputRef

        _ ->
            Cmd.none
    )


handleDisconnection : Model -> ( Model, Cmd Msg )
handleDisconnection model =
    ( { model | ogmiosConnection = OgmiosDisconnected }
    , connectCmd model.websocketAddress
    )


connectCmd : String -> Cmd Msg
connectCmd websocketAddress =
    Ogmios6.connect { connectionId = "from-elm-to-" ++ websocketAddress, websocketAddress = websocketAddress }
        |> Ogmios6.encodeRequest
        |> toOgmios


handleApiResponse : Ogmios6.ApiResponse -> Model -> ( Model, Cmd Msg )
handleApiResponse response model =
    case ( response, model.utxo ) of
        ( Ogmios6.LedgerStateUtxo [ { address, value } ], FetchingUtxoAt outputRef ) ->
            ( { model | utxo = UtxoContents outputRef { address = address, value = value } }
              -- Once we have the public address,
              -- we can easily check the validity of the two keys by deriving the private/public pair
            , Cmd.none
            )

        ( Ogmios6.LedgerStateUtxo _, UtxoContents _ { address } ) ->
            ( model
            , case Debug.log "route in handle" model.route of
                RouteClaim _ { key1, key2 } ->
                    case ( key1, key2 ) of
                        ( Just k1, Just k2 ) ->
                            checkKeysValidity
                                { key1 = k1
                                , key2 = k2
                                , seed23 = k1 :: k2 :: model.words3To23
                                , publicAddress = address
                                }

                        _ ->
                            Cmd.none

                _ ->
                    Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.route of
        RouteHome ->
            viewHome model

        RouteClaim _ { key1, key2 } ->
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
viewClaim maybeKey1 maybeKey2 ({ keyInput, utxo } as model) =
    case ( maybeKey1, maybeKey2 ) of
        ( Nothing, Nothing ) ->
            div []
                [ div [] [ text <| "You found this claim link, congrats!" ]
                , div [] [ text <| "Now let's find the keys to that lock, shall we?" ]
                , div [] [ text <| "Keys to unlock the gift:" ]
                , Html.pre [] (text "   key1: " :: viewMaybeKey maybeKey1 keyInput)
                , Html.pre [] [ text <| "   key2: ?" ]
                , div [] [ text <| "Gift contents:" ]
                , viewUtxoContents utxo
                ]

        ( Just key1, Just key2 ) ->
            div []
                [ div [] [ text <| "You found this claim link, congrats!" ]
                , div [] [ text <| "Wow seems like you found a pair of keys, let's try claiming it!" ]
                , div [] [ text <| "Keys to unlock the gift:" ]
                , Html.pre [] [ text <| "   key1: " ++ key1 ]
                , Html.pre [] [ text <| "   key2: " ++ key2 ]
                , div [] [ text <| "Gift contents:" ]
                , viewUtxoContents utxo
                , viewClaimButton utxo key1 key2 model
                ]

        _ ->
            div []
                [ div [] [ text <| "You found this claim link, congrats!" ]
                , div [] [ text <| "Seems you got your hands on one key. Let's find the other one now! Clue: talk to people and look for the same symbol." ]
                , div [] [ text <| "Keys to unlock the gift:" ]
                , Html.pre [] (text "   key1: " :: viewMaybeKey maybeKey1 keyInput)
                , Html.pre [] (text "   key2: " :: viewMaybeKey maybeKey2 keyInput)
                , div [] [ text <| "Gift contents:" ]
                , viewUtxoContents utxo
                ]


viewClaimButton : UtxoStatus -> String -> String -> Model -> Html Msg
viewClaimButton utxo word1 word2 model =
    case utxo of
        UtxoContents outputRef { address, value } ->
            let
                txFee =
                    -- fee of 0.20 ada
                    N.fromSafeInt 200000

                adaOut =
                    N.sub value.lovelace txFee

                ada1 =
                    N.divBy (N.fromSafeInt 2) adaOut
                        |> Maybe.withDefault N.zero

                ada2 =
                    N.sub adaOut ada1

                -- TODO: add destination address
                dest1 =
                    Address.base Address.Mainnet
                        (Address.VKeyHash <| Bytes.fromStringUnchecked "TODO1 payment Cred")
                        (Address.VKeyHash <| Bytes.fromStringUnchecked "TODO1 stake Cred")

                -- TODO: add destination address
                dest2 =
                    Address.base Address.Mainnet
                        (Address.VKeyHash <| Bytes.fromStringUnchecked "TODO2 payment Cred")
                        (Address.VKeyHash <| Bytes.fromStringUnchecked "TODO2 stake Cred")

                claimTx =
                    Tx.new
                        |> Tx.input outputRef
                        |> Tx.fee txFee
                        |> Tx.payToAddress dest1 ada1
                        |> Tx.payToAddress dest2 ada2
                        |> Tx.complete

                { key1, key2, privateKey, publicAddress, validity } =
                    model.keysValidity
            in
            if key1 /= word1 || key2 /= word2 then
                div [] [ text "Checking the validity of the keys ..." ]

            else if validity == False then
                div [] [ text "Seems you don't have the correct key match to claim this gift, keep looking!" ]

            else
                -- TODO: button that builds Tx and ask to submit via Ogmios
                div []
                    [ text "CONGRATS! (there should be a button to claim here, "
                    , text "but by reason of lastminuteism there isn't). "
                    , text "Instead there is just one seed phrase (you all have the same): "

                    -- TODO: TEMP privateKey is just word 24
                    , text <| String.join " " (key1 :: key2 :: model.words3To23 ++ [ privateKey ])
                    ]

        _ ->
            div [] []


viewUtxoContents : UtxoStatus -> Html msg
viewUtxoContents utxo =
    let
        contents =
            case utxo of
                UnknownUtxoStatus ->
                    "Unknown utxo status, loading ..."

                FetchingUtxoAt _ ->
                    "Loading ..."

                UtxoContents _ { address, value } ->
                    -- IMPROVEMENT: also display native tokens
                    "Address: "
                        ++ address
                        ++ "\n   "
                        ++ "lovelace: "
                        ++ N.toString value.lovelace

                UtxoConsumedAlready ->
                    "Already claimed it seems! :("
    in
    Html.pre [] [ text <| "   " ++ contents ]


viewMaybeKey : Maybe String -> String -> List (Html Msg)
viewMaybeKey maybeKey keyInput =
    case maybeKey of
        Nothing ->
            [ viewInput "text" "dog" keyInput KeyInputChange
            , Html.button [ onClick (CheckKeyInput keyInput) ] [ text "check" ]
            ]

        Just key ->
            [ text key ]


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


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    Html.input [ HA.type_ t, HA.placeholder p, HA.value v, onInput toMsg ] []
