module Ogmios6 exposing
    ( Request, encodeRequest
    , connect, disconnect
    , findIntersection, nextBlock
    , Response(..), ApiResponse(..), responseDecoder
    )

{-| Ogmios 6 support.

@docs Request, encodeRequest

@docs connect, disconnect

@docs findIntersection, nextBlock

@docs Response, ApiResponse, responseDecoder

-}

import Dict exposing (Dict)
import Html.Attributes exposing (id)
import Json.Decode as JDecode exposing (Decoder, Value)
import Json.Encode as JEncode


{-| Opaque type for requests to be sent to Ogmios.
-}
type Request
    = Connect { connectionId : String, websocketAddress : String }
    | Disconnect { ws : Value }
    | ApiRequest
        { ws : Value
        , method : String
        , params : Dict String Value
        , id : String
        }


{-| Encode a [Request] into a JS value that can be sent through a port.
-}
encodeRequest : Request -> Value
encodeRequest request =
    case request of
        Connect { connectionId, websocketAddress } ->
            JEncode.object
                [ ( "requestType", JEncode.string "ogmios-connect" )
                , ( "websocketAddress", JEncode.string websocketAddress )
                , ( "connectionId", JEncode.string connectionId )
                ]

        Disconnect { ws } ->
            JEncode.object
                [ ( "requestType", JEncode.string "ogmios-disconnect" )
                , ( "ws", ws )
                ]

        ApiRequest { ws, method, params, id } ->
            let
                jsonrpcRequest =
                    JEncode.object
                        [ ( "jsonrpc", JEncode.string "2.0" )
                        , ( "method", JEncode.string method )
                        , ( "params", JEncode.dict identity identity params )
                        , ( "id", JEncode.string id )
                        ]
            in
            JEncode.object
                [ ( "requestType", JEncode.string "ogmios-api" )
                , ( "ws", ws )
                , ( "request", jsonrpcRequest )
                ]


connect : { connectionId : String, websocketAddress : String } -> Request
connect config =
    Connect config


disconnect : { ws : Value } -> Request
disconnect =
    Disconnect


findIntersection : { websocket : Value, slot : Int, id : String } -> Request
findIntersection { websocket, slot, id } =
    ApiRequest
        { ws = websocket
        , method = "findIntersection"
        , params =
            Dict.fromList
                [ ( "points"
                  , JEncode.list (JEncode.dict identity identity)
                        [ Dict.fromList
                            [ ( "slot", JEncode.int slot )
                            , ( "id", JEncode.string id )
                            ]
                        ]
                  )
                ]
        , id = "find-intersection"
        }


nextBlock : { websocket : Value } -> Request
nextBlock { websocket } =
    ApiRequest
        { ws = websocket
        , method = "nextBlock"
        , params = Dict.empty
        , id = "next-block"
        }



-- Responses


{-| Response type for responses from Ogmios.
-}
type Response
    = Connected { connectionId : String, ws : Value }
    | Disconnected { connectionId : String }
    | ApiResponse { connectionId : String, requestId : String } ApiResponse
    | Error String
    | UnhandledResponseType String


{-| Response type for all API requests sent to Ogmios.
-}
type ApiResponse
    = IntersectionFound { slot : Int, id : String }


{-| Decoder for the [Response] type.
-}
responseDecoder : Decoder Response
responseDecoder =
    JDecode.field "responseType" JDecode.string
        |> JDecode.andThen
            (\responseType ->
                case responseType of
                    "ogmios-connect" ->
                        connectedDecoder

                    "ogmios-disconnect" ->
                        disconnectedDecoder

                    "ogmios-message" ->
                        JDecode.map2 (\connectionId method -> { connectionId = connectionId, method = method })
                            (JDecode.field "connectionId" JDecode.string)
                            (JDecode.at [ "message", "method" ] JDecode.string)
                            |> JDecode.andThen (JDecode.field "message" << messageDecoder)

                    "ogmios-error" ->
                        JDecode.field "error" JDecode.string
                            |> JDecode.map Error

                    _ ->
                        JDecode.succeed (UnhandledResponseType responseType)
            )


connectedDecoder : Decoder Response
connectedDecoder =
    JDecode.map2 (\ws connectionId -> Connected { ws = ws, connectionId = connectionId })
        (JDecode.field "ws" JDecode.value)
        (JDecode.field "connectionId" JDecode.string)


disconnectedDecoder : Decoder Response
disconnectedDecoder =
    JDecode.map (\connectionId -> Disconnected { connectionId = connectionId })
        (JDecode.field "connectionId" JDecode.string)


messageDecoder : { connectionId : String, method : String } -> Decoder Response
messageDecoder { connectionId, method } =
    case method of
        -- TODO: handle error cases
        "findIntersection" ->
            JDecode.map3
                (\requestId slot id ->
                    ApiResponse { connectionId = connectionId, requestId = requestId } <|
                        IntersectionFound { slot = slot, id = id }
                )
                (JDecode.field "id" JDecode.string)
                (JDecode.at [ "result", "intersection", "slot" ] JDecode.int)
                (JDecode.at [ "result", "intersection", "id" ] JDecode.string)

        _ ->
            JDecode.succeed (UnhandledResponseType method)
