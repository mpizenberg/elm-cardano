module Ogmios6 exposing
    ( Request, encodeRequest
    , connect, disconnect
    , findIntersection, nextBlock
    , queryLedgerStateUtxo
    , Response(..), ApiResponse(..), Block, BlockType(..), responseDecoder
    )

{-| Ogmios 6 support.

@docs Request, encodeRequest

@docs connect, disconnect

@docs findIntersection, nextBlock

@docs queryLedgerStateUtxo

@docs Response, ApiResponse, Block, BlockType, responseDecoder

-}

import Cardano.Value
import Dict exposing (Dict)
import Html.Attributes exposing (id)
import Json.Decode as JDecode exposing (Decoder, Value)
import Json.Encode as JEncode
import Natural


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


queryLedgerStateUtxo : { websocket : Value, txId : String, index : Int } -> Request
queryLedgerStateUtxo { websocket, txId, index } =
    ApiRequest
        { ws = websocket
        , method = "queryLedgerState/utxo"
        , params =
            Dict.fromList
                [ ( "outputReferences"
                  , JEncode.list identity
                        [ JEncode.object
                            [ ( "transaction", JEncode.object [ ( "id", JEncode.string txId ) ] )
                            , ( "index", JEncode.int index )
                            ]
                        ]
                  )
                ]
        , id = "query-utxo"
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


{-| Response type for API requests sent to Ogmios.
-}
type ApiResponse
    = IntersectionFound { slot : Int, id : String }
    | RollBackward { slot : Int, id : String }
    | RollForward Block
    | LedgerStateUtxo (List { txId : String, index : Int, address : String, value : Cardano.Value.Value })


{-| Block data from Ogmios response.
-}
type alias Block =
    { era : String
    , id : String
    , ancestor : String
    , height : Int
    , blockType : BlockType
    }


{-| Block type.
-}
type BlockType
    = EpochBoundaryBlock
    | RegularBlock { slot : Int, transactions : List { id : String, cbor : String } }


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
                        -- TODO: is it guarantied that ogmios returns the request id?
                        JDecode.map3
                            (\connectionId method requestId ->
                                { connectionId = connectionId
                                , method = method
                                , requestId = requestId
                                }
                            )
                            (JDecode.field "connectionId" JDecode.string)
                            (JDecode.at [ "message", "method" ] JDecode.string)
                            (JDecode.at [ "message", "id" ] JDecode.string)
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


messageDecoder : { connectionId : String, method : String, requestId : String } -> Decoder Response
messageDecoder { connectionId, method, requestId } =
    case method of
        -- TODO: handle other cases
        "findIntersection" ->
            JDecode.map2
                (\slot id ->
                    ApiResponse { connectionId = connectionId, requestId = requestId } <|
                        IntersectionFound { slot = slot, id = id }
                )
                (JDecode.at [ "result", "intersection", "slot" ] JDecode.int)
                (JDecode.at [ "result", "intersection", "id" ] JDecode.string)

        "nextBlock" ->
            JDecode.at [ "result", "direction" ] JDecode.string
                |> JDecode.andThen
                    (\direction -> JDecode.field "result" (blockResponseDecoder direction))
                |> JDecode.map (ApiResponse { connectionId = connectionId, requestId = requestId })

        "queryLedgerState/utxo" ->
            JDecode.field "result" (JDecode.list outputRefDecoder)
                |> JDecode.map (ApiResponse { connectionId = connectionId, requestId = requestId } << LedgerStateUtxo)

        _ ->
            JDecode.succeed (UnhandledResponseType method)


outputRefDecoder : Decoder { txId : String, index : Int, address : String, value : Cardano.Value.Value }
outputRefDecoder =
    JDecode.map4
        (\txId index address value -> { txId = txId, index = index, address = address, value = value })
        (JDecode.at [ "transaction", "id" ] JDecode.string)
        (JDecode.field "index" JDecode.int)
        (JDecode.field "address" JDecode.string)
        -- TODO: correctly handle potential big integers
        -- TODO: also decode tokens
        (JDecode.at [ "value", "ada", "lovelace" ] JDecode.int
            |> JDecode.map (Natural.fromSafeInt >> Cardano.Value.onlyLovelace)
        )


blockResponseDecoder : String -> Decoder ApiResponse
blockResponseDecoder direction =
    case direction of
        "forward" ->
            JDecode.field "block" <|
                JDecode.map RollForward blockDecoder

        "backward" ->
            JDecode.map2 (\slot id -> RollBackward { slot = slot, id = id })
                (JDecode.at [ "point", "slot" ] JDecode.int)
                (JDecode.at [ "point", "id" ] JDecode.string)

        _ ->
            JDecode.fail ("Unknown nextBlock direction: " ++ direction)


blockDecoder : Decoder Block
blockDecoder =
    JDecode.map5 Block
        (JDecode.field "era" JDecode.string)
        (JDecode.field "id" JDecode.string)
        (JDecode.field "ancestor" JDecode.string)
        (JDecode.field "height" JDecode.int)
        blockTypeDecoder


blockTypeDecoder : Decoder BlockType
blockTypeDecoder =
    JDecode.field "type" JDecode.string
        |> JDecode.andThen
            (\blockType ->
                case blockType of
                    "ebb" ->
                        JDecode.succeed EpochBoundaryBlock

                    "bft" ->
                        regularBlockDecoder

                    "praos" ->
                        regularBlockDecoder

                    _ ->
                        JDecode.fail ("Unknown block type: " ++ blockType)
            )


regularBlockDecoder : Decoder BlockType
regularBlockDecoder =
    JDecode.map2 (\slot transactions -> RegularBlock { slot = slot, transactions = transactions })
        (JDecode.field "slot" JDecode.int)
        (JDecode.field "transactions" <| JDecode.list transactionDecoder)


transactionDecoder : Decoder { id : String, cbor : String }
transactionDecoder =
    JDecode.map2 (\id cbor -> { id = id, cbor = cbor })
        (JDecode.field "id" JDecode.string)
        (JDecode.field "cbor" JDecode.string)
