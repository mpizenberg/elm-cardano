port module Main exposing (main)

import Browser
import Bytes.Comparable as Bytes
import Cardano.Transaction as Transaction exposing (Transaction)
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Json.Decode as JDecode exposing (Value, value)
import Ogmios6
import Platform.Cmd as Cmd


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> fromOgmios OgmiosMsg
        , view = view
        }


port toOgmios : Value -> Cmd msg


port fromOgmios : (Value -> msg) -> Sub msg


type Msg
    = OgmiosMsg Value
    | WebsocketAddressInputChange String
    | ConnectButtonClicked
    | DisconnectButtonClicked
      -- Find Intersection
    | FindIntersectionButtonClicked
    | FindIntersectionSlotInputChange String
    | FindIntersectionIdInputChange String
      -- Next Block
    | NextBlockButtonClicked
    | PipeliningInputChange String
    | RockRollButtonClicked



-- MODEL


type alias Model =
    { connectionStatus : ConnectionStatus
    , websocketAddress : String

    -- findIntersection form
    , findIntersectionSlot : String
    , findIntersectionId : String

    -- Responses
    , lastApiResponse : String
    , lastError : String

    -- Transactions unrolling
    , nextBlockPipelining : Int
    , unrollingState : UnrollingState
    , lastTwoBlocksProcessed : List { blockHeight : Maybe Int, slot : Int, id : String }
    , failedTxDecoding : Maybe { blockHeight : Int, txId : String, cbor : String, error : String }
    }


type ConnectionStatus
    = Disconnected
    | Connecting
    | Connected { websocket : Value, connectionId : String }


type UnrollingState
    = Idle
    | RockNRolling
    | WaitingForReconnectionThenIntersection { slot : Int, id : String }
    | WaitingForIntersection


init : () -> ( Model, Cmd Msg )
init _ =
    ( { connectionStatus = Disconnected
      , websocketAddress = "ws://0.0.0.0:1337"
      , findIntersectionSlot = "4492799"
      , findIntersectionId = "f8084c61b6a238acec985b59310b6ecec49c0ab8352249afd7268da5cff2a457"
      , lastApiResponse = ""
      , lastError = ""
      , nextBlockPipelining = 1
      , unrollingState = Idle
      , lastTwoBlocksProcessed = []
      , failedTxDecoding = Nothing
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.connectionStatus ) of
        ( OgmiosMsg value, _ ) ->
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

        -- Websocket address input change
        ( WebsocketAddressInputChange address, Disconnected ) ->
            ( { model | websocketAddress = address }, Cmd.none )

        ( WebsocketAddressInputChange _, _ ) ->
            ( model, Cmd.none )

        -- Connect
        ( ConnectButtonClicked, Disconnected ) ->
            ( { model | connectionStatus = Connecting }
            , connectCmd model.websocketAddress
            )

        ( ConnectButtonClicked, _ ) ->
            ( model, Cmd.none )

        -- Disconnect
        ( DisconnectButtonClicked, Connected { websocket } ) ->
            ( { model | connectionStatus = Connecting }
            , Ogmios6.disconnect { ws = websocket }
                |> Ogmios6.encodeRequest
                |> toOgmios
            )

        ( DisconnectButtonClicked, _ ) ->
            ( model, Cmd.none )

        -- Find Intersection
        ( FindIntersectionButtonClicked, Connected { websocket } ) ->
            ( model
            , findIntersectionCmd websocket
                (Maybe.withDefault 0 <| String.toInt model.findIntersectionSlot)
                model.findIntersectionId
            )

        ( FindIntersectionButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( FindIntersectionSlotInputChange slot, _ ) ->
            ( { model | findIntersectionSlot = slot }, Cmd.none )

        ( FindIntersectionIdInputChange id, _ ) ->
            ( { model | findIntersectionId = id }, Cmd.none )

        -- Next Block
        ( NextBlockButtonClicked, Connected { websocket } ) ->
            ( model, nextBlockCmd websocket )

        ( NextBlockButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( PipeliningInputChange pipeliningStr, _ ) ->
            ( { model | nextBlockPipelining = Maybe.withDefault model.nextBlockPipelining <| String.toInt pipeliningStr }
            , Cmd.none
            )

        ( RockRollButtonClicked, Connected { websocket } ) ->
            ( { model | unrollingState = RockNRolling }
            , nextBlockCmd websocket
                |> List.repeat model.nextBlockPipelining
                |> Cmd.batch
            )

        ( RockRollButtonClicked, _ ) ->
            ( model, Cmd.none )


handleConnection : { connectionId : String, ws : Value } -> Model -> ( Model, Cmd Msg )
handleConnection { connectionId, ws } model =
    case model.unrollingState of
        Idle ->
            ( { model | connectionStatus = Connected { websocket = ws, connectionId = connectionId } }
            , Cmd.none
            )

        WaitingForReconnectionThenIntersection { slot, id } ->
            ( { model
                | connectionStatus = Connected { websocket = ws, connectionId = connectionId }
                , unrollingState = WaitingForIntersection
              }
            , findIntersectionCmd ws slot id
            )

        _ ->
            -- should not happen, do nothing
            ( model, Cmd.none )


findIntersectionCmd : Value -> Int -> String -> Cmd Msg
findIntersectionCmd ws slot id =
    Ogmios6.findIntersection
        { websocket = ws
        , slot = slot
        , id = id
        }
        |> Ogmios6.encodeRequest
        |> toOgmios


handleDisconnection : Model -> ( Model, Cmd Msg )
handleDisconnection model =
    case ( model.unrollingState, model.lastTwoBlocksProcessed ) of
        ( RockNRolling, [ _, { slot, id } ] ) ->
            ( { model
                | connectionStatus = Disconnected
                , unrollingState = WaitingForReconnectionThenIntersection { slot = slot, id = id }
              }
            , connectCmd model.websocketAddress
            )

        ( RockNRolling, [ { slot, id } ] ) ->
            ( { model
                | connectionStatus = Disconnected
                , unrollingState = WaitingForReconnectionThenIntersection { slot = slot, id = id }
              }
            , connectCmd model.websocketAddress
            )

        _ ->
            ( { model | connectionStatus = Disconnected }, Cmd.none )


connectCmd : String -> Cmd Msg
connectCmd websocketAddress =
    Ogmios6.connect { connectionId = "from-elm-to-" ++ websocketAddress, websocketAddress = websocketAddress }
        |> Ogmios6.encodeRequest
        |> toOgmios


orElse : Maybe a -> Maybe a -> Maybe a
orElse secondChoice firstChoice =
    if firstChoice == Nothing then
        secondChoice

    else
        firstChoice


handleApiResponse : Ogmios6.ApiResponse -> Model -> ( Model, Cmd Msg )
handleApiResponse response model =
    case ( response, model.failedTxDecoding ) of
        ( _, Just _ ) ->
            -- Do nothing if we encountered a decode failure, to keep the state intact
            ( model, Cmd.none )

        ( Ogmios6.IntersectionFound { slot, id }, Nothing ) ->
            case ( model.unrollingState, model.connectionStatus ) of
                ( WaitingForIntersection, Connected { websocket } ) ->
                    ( { model
                        | lastTwoBlocksProcessed = [ { blockHeight = Nothing, slot = slot, id = id } ]
                        , unrollingState = RockNRolling
                      }
                    , nextBlockCmd websocket
                        |> List.repeat model.nextBlockPipelining
                        |> Cmd.batch
                    )

                ( _, _ ) ->
                    ( { model | lastTwoBlocksProcessed = [ { blockHeight = Nothing, slot = slot, id = id } ] }
                    , Cmd.none
                    )

        ( Ogmios6.RollBackward { slot, id }, Nothing ) ->
            ( { model | lastTwoBlocksProcessed = [ { blockHeight = Nothing, slot = slot, id = id } ] }
            , rerollCmd model.connectionStatus model.unrollingState
            )

        ( Ogmios6.RollForward { id, height, blockType }, Nothing ) ->
            case blockType of
                Ogmios6.EpochBoundaryBlock ->
                    ( model, rerollCmd model.connectionStatus model.unrollingState )

                Ogmios6.RegularBlock { slot, transactions } ->
                    let
                        lastTwo =
                            { blockHeight = Just height, slot = slot, id = id }
                                :: model.lastTwoBlocksProcessed
                                |> List.take 2

                        failedTxDecodingAttempt =
                            List.map attemptDecode transactions
                                |> List.filterMap getError
                                |> List.head
                    in
                    case failedTxDecodingAttempt of
                        Nothing ->
                            ( { model | lastTwoBlocksProcessed = lastTwo }
                            , rerollCmd model.connectionStatus model.unrollingState
                            )

                        Just { txId, cbor, error } ->
                            ( { model
                                | lastTwoBlocksProcessed = lastTwo
                                , unrollingState = Idle
                                , failedTxDecoding =
                                    Just
                                        { blockHeight = height
                                        , txId = txId
                                        , cbor = cbor
                                        , error = error
                                        }
                              }
                            , Cmd.none
                            )


attemptDecode : { id : String, cbor : String } -> Result { txId : String, cbor : String, error : String } Transaction
attemptDecode { id, cbor } =
    Transaction.deserialize (Bytes.fromStringUnchecked cbor)
        |> Result.fromMaybe { txId = id, cbor = cbor, error = "Failed to decode" }


getError : Result err Transaction -> Maybe err
getError result =
    case result of
        Err err ->
            Just err

        Ok _ ->
            Nothing


rerollCmd : ConnectionStatus -> UnrollingState -> Cmd Msg
rerollCmd connectionStatus unrollingState =
    case ( connectionStatus, unrollingState ) of
        ( Connected { websocket }, RockNRolling ) ->
            nextBlockCmd websocket

        -- Process.sleep 10000 |> Task.perform (always NextBlockButtonClicked)
        _ ->
            Cmd.none


nextBlockCmd : Value -> Cmd Msg
nextBlockCmd websocket =
    Ogmios6.nextBlock { websocket = websocket }
        |> Ogmios6.encodeRequest
        |> toOgmios



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewConnectionStatus model
        , viewFindIntersection model
        , viewNextBlock model
        , div [] [ text "Last API request response:" ]
        , Html.pre [] [ text model.lastApiResponse ]
        , div [] [ text "Last error:" ]
        , Html.pre [] [ text model.lastError ]
        , div []
            [ text "pipelining:"
            , viewInput "text" "1" (String.fromInt model.nextBlockPipelining) PipeliningInputChange
            , Html.button [ onClick RockRollButtonClicked ] [ text <| "Rock'N'Roll!" ]
            ]
        , viewLastTwoBlocksProcessed model.lastTwoBlocksProcessed
        , viewLatestFailedTxDecoding model.failedTxDecoding
        ]


viewConnectionStatus : Model -> Html Msg
viewConnectionStatus { connectionStatus, websocketAddress } =
    case connectionStatus of
        Disconnected ->
            div []
                [ Html.button [ onClick ConnectButtonClicked ] [ text <| "Connect to: " ]
                , viewInput "text" "ws://0.0.0.0:1337" websocketAddress WebsocketAddressInputChange
                ]

        Connecting ->
            div [] [ text <| "Connecting to " ++ websocketAddress ++ " ..." ]

        Connected { connectionId } ->
            div []
                [ text <| "Connected | ID: " ++ connectionId
                , Html.button [ onClick DisconnectButtonClicked ] [ text <| "Disconnect" ]
                ]


viewFindIntersection : Model -> Html Msg
viewFindIntersection { findIntersectionSlot, findIntersectionId } =
    div []
        [ Html.button [ onClick FindIntersectionButtonClicked ] [ text <| "Find Intersection" ]
        , viewInput "text" "4492799" findIntersectionSlot FindIntersectionSlotInputChange
        , viewInput "text" "f8084c61b6a238acec985b59310b6ecec49c0ab8352249afd7268da5cff2a457" findIntersectionId FindIntersectionIdInputChange
        ]


viewNextBlock : Model -> Html Msg
viewNextBlock _ =
    div []
        [ Html.button [ onClick NextBlockButtonClicked ] [ text <| "Next Block" ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    Html.input [ HA.type_ t, HA.placeholder p, HA.value v, onInput toMsg ] []


viewLastTwoBlocksProcessed : List { blockHeight : Maybe Int, slot : Int, id : String } -> Html Msg
viewLastTwoBlocksProcessed lastTwoBlocksProcessed =
    lastTwoBlocksProcessed
        |> List.map
            (\{ blockHeight, slot, id } ->
                "   height: "
                    ++ (Maybe.map String.fromInt blockHeight |> Maybe.withDefault "?")
                    ++ ", slot: "
                    ++ String.fromInt slot
                    ++ ", id: "
                    ++ id
            )
        |> (::) "Last (max 2) blocks processed:"
        |> String.join "\n"
        |> (List.singleton << text)
        |> Html.pre []


viewLatestFailedTxDecoding : Maybe { blockHeight : Int, txId : String, cbor : String, error : String } -> Html Msg
viewLatestFailedTxDecoding maybeError =
    case maybeError of
        Nothing ->
            div [] []

        Just { blockHeight, txId, cbor, error } ->
            div []
                [ div [] [ text <| "In block height: " ++ String.fromInt blockHeight ]
                , div [] [ text <| "Failed to decode transaction " ++ txId ]
                , div [] [ text <| "Tx CBOR: " ++ cbor ]
                , div [] [ text <| "Decoding error:" ]
                , Html.pre [] [ text error ]
                ]
