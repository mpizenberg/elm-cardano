port module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
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
    | ConnectButtonClicked
    | DisconnectButtonClicked
    | FindIntersectionButtonClicked



-- MODEL


type alias Model =
    { connectionStatus : ConnectionStatus
    , websocketAddress : String
    , lastApiResponse : String
    , lastError : String
    }


type ConnectionStatus
    = Disconnected
    | Connecting
    | Connected { websocket : Value, connectionId : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { connectionStatus = Disconnected, websocketAddress = "ws://0.0.0.0:1337", lastApiResponse = "", lastError = "" }
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
                    ( { model | lastApiResponse = Debug.toString response }, Cmd.none )

                Ok (Ogmios6.Error error) ->
                    ( { model | lastError = error }, Cmd.none )

                Ok (Ogmios6.UnhandledResponseType error) ->
                    ( { model | lastError = error }, Cmd.none )

                Err error ->
                    ( { model | lastError = JDecode.errorToString error }, Cmd.none )

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
            , Ogmios6.findIntersection { websocket = websocket }
                |> Ogmios6.encodeRequest
                |> toOgmios
            )

        ( FindIntersectionButtonClicked, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewConnectionStatus model
        , div [] [ Html.button [ onClick FindIntersectionButtonClicked ] [ text <| "findIntersection(lastByronBlock)" ] ]
        , div [] [ text "Last API request response:" ]
        , Html.pre [] [ text model.lastApiResponse ]
        , div [] [ text "Last error:" ]
        , Html.pre [] [ text model.lastError ]
        ]


viewConnectionStatus : Model -> Html Msg
viewConnectionStatus { connectionStatus, websocketAddress } =
    case connectionStatus of
        Disconnected ->
            div [] [ Html.button [ onClick ConnectButtonClicked ] [ text <| "Connect to: " ++ websocketAddress ] ]

        Connecting ->
            div [] [ text <| "Connecting to " ++ websocketAddress ++ " ..." ]

        Connected { connectionId } ->
            div []
                [ text <| "Connected | ID: " ++ connectionId
                , Html.button [ onClick DisconnectButtonClicked ] [ text <| "Disconnect" ]
                ]
