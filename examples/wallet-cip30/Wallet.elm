module Wallet exposing
    ( Cip30Request
    , Cip30Response(..)
    , Cip30Wallet
    , Cip30WalletDescriptor
    , cip30ResponseDecoder
    , discoverCip30Wallets
    , encodeCip30Request
    )

import Json.Decode as JDecode exposing (Decoder, Value)
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
        , args : List ( String, Value )
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
    case ( amount, paginate ) of
        ( Nothing, Nothing ) ->
            cip30ApiRequest wallet "getUtxos" []

        ( Just limitAmount, Nothing ) ->
            cip30ApiRequest wallet "getUtxos" [ ( "amount", encodeLimitAmount limitAmount ) ]

        ( Nothing, Just thePage ) ->
            cip30ApiRequest wallet "getUtxos" [ ( "paginate", encodePaginate thePage ) ]

        ( Just limitAmount, Just thePage ) ->
            cip30ApiRequest wallet "getUtxos" [ ( "amount", encodeLimitAmount limitAmount ), ( "paginate", encodePaginate thePage ) ]


type alias Paginate =
    { page : Int, limit : Int }


encodePaginate : Paginate -> Value
encodePaginate { page, limit } =
    JEncode.object [ ( "page", JEncode.int page ), ( "limit", JEncode.int limit ) ]


encodeLimitAmount : TODO -> Value
encodeLimitAmount _ =
    Debug.todo "encodeLimitAmount"


cip30ApiRequest : Cip30Wallet -> String -> List ( String, Value ) -> Cip30Request
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
                , ( "args", JEncode.object args )
                ]


type Cip30Response
    = AvailableCip30Wallets (List Cip30WalletDescriptor)
    | EnablingError { id : String } ApiError
    | EnabledCip30Wallet Cip30Wallet
    | NetworkId { walletId : String, networkId : Int }
    | UnhandledResponseType String


type ApiError
    = ApiError


cip30ResponseDecoder : Decoder Cip30Response
cip30ResponseDecoder =
    JDecode.field "responseType" JDecode.string
        |> JDecode.andThen
            (\responseType ->
                case responseType of
                    "cip30-discover" ->
                        discoverDecoder

                    "cip30-enable" ->
                        enableDecoder ()

                    "cip30-api" ->
                        apiDecoder ()

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


enableDecoder : () -> Decoder Cip30Response
enableDecoder _ =
    Debug.todo "enableDecoder -> EnablingError {id} ApiError | EnabledCip30Wallet Cip30Wallet"


apiDecoder : () -> Decoder Cip30Response
apiDecoder _ =
    Debug.todo "apiDecoder -> depends on the method"



-- api.getExtensions()
-- api.getNetworkId()
-- api.getUtxos(amount: cbor\ = undefined, paginate: Paginate = undefined)
-- api.getCollateral(params: { amount: cbor\ })
-- api.getBalance()
-- api.getUsedAddresses(paginate: Paginate = undefined)
-- api.getUnusedAddresses()
-- api.getChangeAddress()
-- api.getRewardAddresses()
--
-- api.signTx(tx: cbor\, partialSign: bool = false)
-- api.signData(addr: Address, payload: Bytes)
-- api.submitTx(tx: cbor\)
