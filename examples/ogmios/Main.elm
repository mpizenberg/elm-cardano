port module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)
import Json.Decode as JDecode exposing (Value, value)
import Ogmios6
import Platform.Cmd as Cmd
import Process
import Task


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
    , unrolledTxs : List String
    }


type ConnectionStatus
    = Disconnected
    | Connecting
    | Connected { websocket : Value, connectionId : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { connectionStatus = Disconnected
      , websocketAddress = "ws://0.0.0.0:1337"
      , findIntersectionSlot = "4492799"
      , findIntersectionId = "f8084c61b6a238acec985b59310b6ecec49c0ab8352249afd7268da5cff2a457"
      , lastApiResponse = ""
      , lastError = ""
      , unrolledTxs = []
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.connectionStatus ) of
        ( OgmiosMsg value, _ ) ->
            case JDecode.decodeValue Ogmios6.responseDecoder value of
                Ok (Ogmios6.Connected { connectionId, ws }) ->
                    ( { model | connectionStatus = Connected { websocket = ws, connectionId = connectionId } }
                    , Cmd.none
                    )

                Ok (Ogmios6.Disconnected _) ->
                    ( { model | connectionStatus = Disconnected }, Cmd.none )

                Ok (Ogmios6.ApiResponse _ response) ->
                    ( { model
                        | lastApiResponse = Debug.toString response
                        , unrolledTxs = updateUnrolledTxs response model.unrolledTxs
                      }
                    , Cmd.none
                    )

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
            , Ogmios6.connect { connectionId = "from-elm-to-" ++ model.websocketAddress, websocketAddress = model.websocketAddress }
                |> Ogmios6.encodeRequest
                |> toOgmios
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
            , Ogmios6.findIntersection
                { websocket = websocket
                , slot = Maybe.withDefault 0 <| String.toInt model.findIntersectionSlot
                , id = model.findIntersectionId
                }
                |> Ogmios6.encodeRequest
                |> toOgmios
            )

        ( FindIntersectionButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( FindIntersectionSlotInputChange slot, _ ) ->
            ( { model | findIntersectionSlot = slot }, Cmd.none )

        ( FindIntersectionIdInputChange id, _ ) ->
            ( { model | findIntersectionId = id }, Cmd.none )

        -- Next Block
        ( NextBlockButtonClicked, Connected { websocket } ) ->
            ( model
            , Ogmios6.nextBlock { websocket = websocket }
                |> Ogmios6.encodeRequest
                |> toOgmios
            )

        ( NextBlockButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( RockRollButtonClicked, Connected _ ) ->
            ( model
            , Cmd.batch
                [ Task.succeed NextBlockButtonClicked |> Task.perform identity
                , Process.sleep 2000 |> Task.perform (always RockRollButtonClicked)
                ]
            )

        ( RockRollButtonClicked, _ ) ->
            ( model, Cmd.none )


updateUnrolledTxs : Ogmios6.ApiResponse -> List String -> List String
updateUnrolledTxs response txs =
    case response of
        Ogmios6.IntersectionFound _ ->
            []

        Ogmios6.RollBackward { slot, id } ->
            [ "RollBack {slot:" ++ String.fromInt slot ++ " , id:" ++ id ++ " }" ]

        Ogmios6.RollForward { era, height, blockType } ->
            case blockType of
                Ogmios6.EpochBoundaryBlock ->
                    ("======= Epoch boundary !!! " ++ era ++ " =======") :: txs

                Ogmios6.RegularBlock { transactions } ->
                    ("block " ++ String.fromInt height)
                        :: List.map ((++) "   tx: ") transactions
                        ++ txs



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
        , div [] [ Html.button [ onClick RockRollButtonClicked ] [ text <| "Rock'N'Roll! (2s loop)" ] ]
        , div [] [ text "Transactions unrolling:" ]
        , Html.pre [] [ text <| String.join "\n" model.unrolledTxs ]
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
