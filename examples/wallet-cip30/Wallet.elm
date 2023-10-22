module Wallet exposing
    ( Cip30Request
    , Cip30Response(..)
    , Cip30Wallet
    , Cip30WalletDescriptor
    , cip30ResponseDecoder
    , cip30WalletDescriptor
    , discoverCip30Wallets
    , enableCip30Wallet
    , encodeCip30Request
    , getBalance
    , getChangeAddress
    , getNetworkId
    , getRewardAddresses
    , getUnusedAddresses
    , getUsedAddresses
    , getUtxos
    )

import Cbor exposing (CborItem)
import Cbor.Decode
import Hex.Convert
import Json.Decode as JDecode exposing (Decoder, Value, maybe)
import Json.Encode as JEncode


type TODO
    = TODO


{-| The type returned when asking for available wallets.
-}
type alias Cip30WalletDescriptor =
    { id : String
    , name : String
    , icon : String
    , apiVersion : String
    , isEnabled : Bool
    , supportedExtensions : List Int
    }


type Cip30Wallet
    = Cip30Wallet
        { descriptor : Cip30WalletDescriptor
        , api : Value
        , walletHandle : Value
        }


cip30WalletDescriptor : Cip30Wallet -> Cip30WalletDescriptor
cip30WalletDescriptor (Cip30Wallet { descriptor }) =
    descriptor


type Cip30Request
    = DiscoverCip30Wallets
    | Cip30Enable { id : String, extensions : List Int }
    | Cip30ApiRequest
        { id : String
        , api : Value
        , method : String
        , args : List Value
        }


discoverCip30Wallets : Cip30Request
discoverCip30Wallets =
    DiscoverCip30Wallets


enableCip30Wallet : { id : String, extensions : List Int } -> Cip30Request
enableCip30Wallet idAndExtensions =
    Cip30Enable idAndExtensions


getExtensions : Cip30Wallet -> Cip30Request
getExtensions wallet =
    cip30ApiRequest wallet "getExtensions" []


getNetworkId : Cip30Wallet -> Cip30Request
getNetworkId wallet =
    cip30ApiRequest wallet "getNetworkId" []


getUtxos : Cip30Wallet -> { amount : Maybe TODO, paginate : Maybe Paginate } -> Cip30Request
getUtxos wallet { amount, paginate } =
    cip30ApiRequest wallet
        "getUtxos"
        [ encodeMaybe encodeLimitAmount amount
        , encodeMaybe encodePaginate paginate
        ]


encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encode maybe =
    Maybe.map encode maybe
        |> Maybe.withDefault JEncode.null


getBalance : Cip30Wallet -> Cip30Request
getBalance wallet =
    cip30ApiRequest wallet "getBalance" []


getUsedAddresses : Cip30Wallet -> { paginate : Maybe Paginate } -> Cip30Request
getUsedAddresses wallet { paginate } =
    cip30ApiRequest wallet "getUsedAddresses" [ encodeMaybe encodePaginate paginate ]


getUnusedAddresses : Cip30Wallet -> Cip30Request
getUnusedAddresses wallet =
    cip30ApiRequest wallet "getUnusedAddresses" []


getChangeAddress : Cip30Wallet -> Cip30Request
getChangeAddress wallet =
    cip30ApiRequest wallet "getChangeAddress" []


getRewardAddresses : Cip30Wallet -> Cip30Request
getRewardAddresses wallet =
    cip30ApiRequest wallet "getRewardAddresses" []



-- api.getExtensions() // avoid for now
-- api.getCollateral(params: { amount: cbor\ })
-- api.getRewardAddresses()
--
-- api.signTx(tx: cbor\, partialSign: bool = false)
-- api.signData(addr: Address, payload: Bytes)
-- api.submitTx(tx: cbor\)


type alias Paginate =
    { page : Int, limit : Int }


encodePaginate : Paginate -> Value
encodePaginate { page, limit } =
    JEncode.object [ ( "page", JEncode.int page ), ( "limit", JEncode.int limit ) ]


encodeLimitAmount : TODO -> Value
encodeLimitAmount _ =
    Debug.todo "encodeLimitAmount"


cip30ApiRequest : Cip30Wallet -> String -> List Value -> Cip30Request
cip30ApiRequest (Cip30Wallet { descriptor, api }) method args =
    Cip30ApiRequest
        { id = descriptor.id
        , api = api
        , method = method
        , args = args
        }


encodeCip30Request : Cip30Request -> Value
encodeCip30Request request =
    case request of
        DiscoverCip30Wallets ->
            JEncode.object
                [ ( "requestType", JEncode.string "cip30-discover" ) ]

        Cip30Enable { id, extensions } ->
            JEncode.object
                [ ( "requestType", JEncode.string "cip30-enable" )
                , ( "id", JEncode.string id )
                , ( "extensions", JEncode.list JEncode.int extensions )
                ]

        Cip30ApiRequest { id, api, method, args } ->
            JEncode.object
                [ ( "requestType", JEncode.string "cip30-api" )
                , ( "id", JEncode.string id )
                , ( "api", api )
                , ( "method", JEncode.string method )
                , ( "args", JEncode.list identity args )
                ]


type Cip30Response
    = AvailableCip30Wallets (List Cip30WalletDescriptor)
    | EnablingError { id : String } ApiError
    | EnabledCip30Wallet Cip30Wallet
    | NetworkId { walletId : String, networkId : Int }
    | WalletUtxos { walletId : String, utxos : List String }
    | WalletBalance { walletId : String, balance : CborItem }
    | UsedAddresses { walletId : String, usedAddresses : List String }
    | UnusedAddresses { walletId : String, unusedAddresses : List String }
    | ChangeAddress { walletId : String, changeAddress : String }
    | RewardAddresses { walletId : String, rewardAddresses : List String }
    | UnhandledResponseType String


type ApiError
    = ApiError String


cip30ResponseDecoder : Decoder Cip30Response
cip30ResponseDecoder =
    JDecode.field "responseType" JDecode.string
        |> JDecode.andThen
            (\responseType ->
                case responseType of
                    "cip30-discover" ->
                        discoverDecoder

                    "cip30-enable" ->
                        enableDecoder

                    "cip30-api" ->
                        JDecode.field "method" JDecode.string
                            |> JDecode.andThen
                                (\method ->
                                    JDecode.field "walletId" JDecode.string
                                        |> JDecode.andThen (apiDecoder method)
                                )

                    _ ->
                        JDecode.succeed (UnhandledResponseType responseType)
            )


discoverDecoder : Decoder Cip30Response
discoverDecoder =
    JDecode.list cip30DescriptorDecoder
        |> JDecode.field "wallets"
        |> JDecode.map AvailableCip30Wallets


cip30DescriptorDecoder : Decoder Cip30WalletDescriptor
cip30DescriptorDecoder =
    JDecode.map6
        -- Explicit constructor with names instead of using Cip30WalletDescriptor constructor
        (\id name icon apiVersion isEnabled supportedExtensions ->
            { id = id
            , name = name
            , icon = icon
            , apiVersion = apiVersion
            , isEnabled = isEnabled
            , supportedExtensions = supportedExtensions
            }
        )
        (JDecode.field "id" JDecode.string)
        (JDecode.field "name" JDecode.string)
        (JDecode.field "icon" JDecode.string)
        (JDecode.field "apiVersion" JDecode.string)
        (JDecode.field "isEnabled" JDecode.bool)
        (JDecode.field "supportedExtensions" (JDecode.list JDecode.int))


enableDecoder : Decoder Cip30Response
enableDecoder =
    -- TODO: Handle the potential errors
    JDecode.map EnabledCip30Wallet <|
        JDecode.map3
            -- Explicit constructor to avoid messing with fields order
            (\descriptor api walletHandle ->
                Cip30Wallet
                    { descriptor = descriptor
                    , api = api
                    , walletHandle = walletHandle
                    }
            )
            (JDecode.field "descriptor" cip30DescriptorDecoder)
            (JDecode.field "api" JDecode.value)
            (JDecode.field "walletHandle" JDecode.value)


apiDecoder : String -> String -> Decoder Cip30Response
apiDecoder method walletId =
    case method of
        "getNetworkId" ->
            JDecode.map (\n -> NetworkId { walletId = walletId, networkId = n })
                (JDecode.field "response" JDecode.int)

        "getUtxos" ->
            JDecode.list (JDecode.succeed "TODO: utxo decoder")
                |> JDecode.field "response"
                |> JDecode.map (\utxos -> WalletUtxos { walletId = walletId, utxos = utxos })

        "getBalance" ->
            JDecode.map (\b -> WalletBalance { walletId = walletId, balance = b })
                (JDecode.field "response" <| hexCborDecoder Cbor.Decode.any)

        "getUsedAddresses" ->
            JDecode.map (\r -> UsedAddresses { walletId = walletId, usedAddresses = r })
                (JDecode.field "response" <| JDecode.list JDecode.string)

        "getUnusedAddresses" ->
            JDecode.map (\r -> UnusedAddresses { walletId = walletId, unusedAddresses = r })
                (JDecode.field "response" <| JDecode.list JDecode.string)

        "getChangeAddress" ->
            JDecode.map (\r -> ChangeAddress { walletId = walletId, changeAddress = r })
                (JDecode.field "response" JDecode.string)

        "getRewardAddresses" ->
            JDecode.map (\r -> RewardAddresses { walletId = walletId, rewardAddresses = r })
                (JDecode.field "response" <| JDecode.list JDecode.string)

        _ ->
            JDecode.succeed <| UnhandledResponseType ("Unknown API call: " ++ method)


hexCborDecoder : Cbor.Decode.Decoder a -> Decoder a
hexCborDecoder decoder =
    JDecode.string
        |> JDecode.andThen
            (\str ->
                case Hex.Convert.toBytes str of
                    Just bytes ->
                        case Cbor.Decode.decode decoder bytes of
                            Just a ->
                                JDecode.succeed a

                            Nothing ->
                                JDecode.fail "Failed to decode CBOR"

                    Nothing ->
                        JDecode.fail "Invalid hex bytes"
            )
