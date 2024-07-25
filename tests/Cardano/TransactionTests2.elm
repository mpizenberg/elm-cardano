module Cardano.TransactionTests2 exposing (suite)

import Bytes.Comparable as Bytes
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Credential(..), NetworkId(..))
import Cardano.Data exposing (Data(..))
import Cardano.Redeemer exposing (RedeemerTag(..))
import Cardano.Script exposing (NativeScript(..))
import Cardano.Transaction as Transaction exposing (Nonce(..), TransactionBody, WitnessSet)
import Cardano.Transaction.AuxiliaryData exposing (AuxiliaryData)
import Cardano.Transaction.AuxiliaryData.Metadatum as Metadatum
import Cardano.Transaction.Builder as Tx
import Cardano.Utxo as Utxo exposing (DatumOption(..))
import Cardano.Value as Value
import Dict exposing (Dict)
import Expect
import Integer
import Natural as N exposing (Natural)
import Test exposing (Test, describe, test)
import Tests exposing (expectBytes)


suite : Test
suite =
    describe "Cardano.Transaction (follow up)"
        [ describe "deserialize (follow up)"
            -- Alonzo transactions
            [ decode8a8f8dfe
            , decodebf095309
            ]
        ]


{-| First Alonzo failure.

Tx id: 8a8f8dfe180112e5a5080078a14528cf8f2e42cc2aeecb5a5c9912efe870991c
Block height: 6236063
Previous block intersection:

  - slot: 39917142
  - id: 1464f8f784e4c9237a08d2068cc37a09598ee69a51748bcf70ab5cf39aa2e106

-}
decode8a8f8dfe : Test
decode8a8f8dfe =
    test "Tx id 8a8f8dfe180112e5a5080078a14528cf8f2e42cc2aeecb5a5c9912efe870991c" <|
        \_ ->
            Bytes.fromStringUnchecked "84a6008482582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e10082582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e10182582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e10282582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1030d8182582041b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e103018482581d6108f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b8891a02a0d28883581d71e265b741cd9ac2c4d695402f7bb4cb49cbe0d9e33eec9a26dbfa9e59821a004c4b40a1581cfb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8a1444641524d01582060cc87c3d3c639bb292b00670f83a08cb6b4abd20e018b137377b34ccf2ccf2982581d6108f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889821a02c2730da1581cfb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8a144434f524e0182581d6108f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b8891a001e8480021a00031c050e8007582036663d429bded43331a968fcaa3a0aba03d6d83474176b8c85a019b0b408ff8da100818258202a61b85e796ce56b7cb960696f27caf8867e3a209fb5243e935ac3bf99aa0c0c58406ee6dfafc7fdcd553bf0c11cc93d165c77a93a265af250eafb1dca2b044ae0b62d6eb4969e7946c438be5f73b0d9a25ad83d074c8d9cd6f4c80ace7b7c62ab0df5d90103a100a11902a2a1636d736783783157656c636f6d6520746f207468652050494759204f7261636c6520616e6420746f2074686520416c6f6e7a6f206572612160781c68747470733a2f2f6f7261636c652e70696779746f6b656e2e636f6d"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = body8a8f8dfe
                        , witnessSet = witnessSet8a8f8dfe
                        , isValid = True
                        , auxiliaryData = Just auxiliaryData8a8f8dfe
                        }
                    )


auxiliaryData8a8f8dfe : AuxiliaryData
auxiliaryData8a8f8dfe =
    { labels = [ ( N.fromSafeInt 674, Metadatum.Map [ ( Metadatum.String "msg", Metadatum.List [ Metadatum.String "Welcome to the PIGY Oracle and to the Alonzo era!", Metadatum.String "", Metadatum.String "https://oracle.pigytoken.com" ] ) ] ) ]
    , nativeScripts = []
    , plutusV1Scripts = []
    , plutusV2Scripts = []
    }


body8a8f8dfe : TransactionBody
body8a8f8dfe =
    { newTxBody
        | auxiliaryDataHash = Just (Bytes.fromStringUnchecked "36663d429bded43331a968fcaa3a0aba03d6d83474176b8c85a019b0b408ff8d")
        , fee = Just (N.fromSafeInt 203781)
        , inputs =
            [ { outputIndex = 0, transactionId = Bytes.fromStringUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            , { outputIndex = 1, transactionId = Bytes.fromStringUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            , { outputIndex = 2, transactionId = Bytes.fromStringUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            , { outputIndex = 3, transactionId = Bytes.fromStringUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" }
            ]
        , collateral = [ { outputIndex = 3, transactionId = Bytes.fromStringUnchecked "41b2bf9badec879f09dc9154c582da12f0c5a7970cd3f922ed34baf83e19e2e1" } ]
        , outputs =
            [ { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "08f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (N.fromSafeInt 44094088)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = ScriptHash (Bytes.fromStringUnchecked "e265b741cd9ac2c4d695402f7bb4cb49cbe0d9e33eec9a26dbfa9e59"), stakeCredential = Nothing }
              , amount = { assets = bytesMap (Dict.fromList [ ( "fb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8", bytesMap (Dict.fromList [ ( "4641524d", N.fromSafeInt 1 ) ]) ) ]), lovelace = N.fromSafeInt 5000000 }
              , datumOption = Just (DatumHash (Bytes.fromStringUnchecked "60cc87c3d3c639bb292b00670f83a08cb6b4abd20e018b137377b34ccf2ccf29"))
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "08f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889"), stakeCredential = Nothing }
              , amount = { assets = bytesMap (Dict.fromList [ ( "fb353334ac071f79f6d938e0972640a6b6650815124e9d63fbc0b5e8", bytesMap (Dict.fromList [ ( "434f524e", N.fromSafeInt 1 ) ]) ) ]), lovelace = N.fromSafeInt 46297869 }
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            , { address = Address.Shelley { networkId = Mainnet, paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "08f30ea42a1e844e389e7e0483e69376dc8babd01ad91ab536c8b889"), stakeCredential = Nothing }
              , amount = Value.onlyLovelace (N.fromSafeInt 2000000)
              , datumOption = Nothing
              , referenceScript = Nothing
              }
            ]
    }


witnessSet8a8f8dfe : WitnessSet
witnessSet8a8f8dfe =
    { newTxWitnessSet
        | vkeywitness =
            Just
                [ { signature = Bytes.fromStringUnchecked "6ee6dfafc7fdcd553bf0c11cc93d165c77a93a265af250eafb1dca2b044ae0b62d6eb4969e7946c438be5f73b0d9a25ad83d074c8d9cd6f4c80ace7b7c62ab0d"
                  , vkey = Bytes.fromStringUnchecked "2a61b85e796ce56b7cb960696f27caf8867e3a209fb5243e935ac3bf99aa0c0c"
                  }
                ]
    }


{-| Next Alonzo failure.

Tx id: bf095309ba20174d1a5c30ea03580cbf8bfe7dd75da1203d9ed51bfd151bb327
Block height: 6555637
Previous block intersection:

  - slot: 46461491
  - id: c35b68abe2bf17cd393c2e3024f751c5343732c9ae83f1c5b3d56a713c7d986d

-}
decodebf095309 : Test
decodebf095309 =
    test "Tx id bf095309ba20174d1a5c30ea03580cbf8bfe7dd75da1203d9ed51bfd151bb327" <|
        \_ ->
            Bytes.fromStringUnchecked "84a6008182582003b02cff29a5f2dfc827e00345eaab8b29a3d740e9878aa6e5dd2b52da0763c5000d80018182581d61d80fe69ded1ff90f41e526d0332a2ff98ba8a0d85ceb8941b51784201a05633249021a00033a9d0682a7581c162f94554ac8c225383a2248c245659eda870eaa82d0ef25fc7dcd82a2021a0001200014821a00aba9501b00000002540be400581c2075a095b3c844a29c24317a94a643ab8e22d54a3a3a72a420260af6a2021a0001200014821a00aba9501b00000002540be400581c268cfc0b89e910ead22e0ade91493d8212f53f3e2164b2e4bef0819ba2021a0001200014821a00aba9501b00000002540be400581c60baee25cbc90047e83fd01e1e57dc0b06d3d0cb150d0ab40bbfead1a2021a0001200014821a00aba9501b00000002540be400581cad5463153dc3d24b9ff133e46136028bdc1edbb897f5a7cf1b37950ca2021a0001200014821a00aba9501b00000002540be400581cb9547b8a57656539a8d9bc42c008e38d9c8bd9c8adbb1e73ad529497a2021a0001200014821a00aba9501b00000002540be400581cf7b341c14cd58fca4195a9b278cce1ef402dc0e06deb77e543cd1757a2021a0001200014821a00aba9501b00000002540be4001901310e80a1008882582061261a95b7613ee6bf2067dad77b70349729b0c50d57bc1cf30de0db4a1e73a85840c65d631ecb286668eeef3537c279fb0c5c5d54bb7ab71a6d0c795f48f6093e664f9e923fd590e3373dd9e054eb622724cb107673a83ad201f503622cdcdae6038258209180d818e69cd997e34663c418a648c076f2e19cd4194e486e159d8580bc6cda584030f64d310d2cc64178fd86681013ba0960d76f5c404d434833238ed2f73a7336fb271027239f0ad0c99436852c023ee95b68d99f02b9956db5776abc8379320a82582089c29f8c4af27b7accbe589747820134ebbaa1caf3ce949270a3d0c7dcfd541b5840fe9cafe18e8fe6c31c2c88012aede78b220857d854a6f488f49aa993aef14d891548c3fd1c2c6c8edfb749c999be26f716991447ae2c0461fa6d7a8d573b0a02825820f14f712dc600d793052d4842d50cefa4e65884ea6cf83707079eb8ce302efc85584001b76c22a07514dfd86182e350e94789c04ff868fc0351a7c74fb58ca6642a658ea1e56c373fa2de85fd1a47bad40f25a62f2caee709d40368ffc14a223171018258208b53207629f9a30e4b2015044f337c01735abe67243c19470c9dae8c7b7327985840fa2b5fc33a08f863c47a67bc5f6fc649b83f0b650ec7ed9329b11db77c562262bd15d5fc97ee71efd4b2df98db9ea133a05aa7f04955d4b21a0c086acc0479018258205fddeedade2714d6db2f9e1104743d2d8d818ecddc306e176108db14caadd4415840ee63870c4c1f27866073b05b5dd418f21712667739d289997c01fbbf28ff0cb3987629588cb13a5459844066d526ce0cde89dc26af5b8108654b0514f7578f0a825820cbc6b506e94fbefe442eecee376f3b3ebaf89415ef5cd2efb666e06ddae483935840a2da7ce96693480cd49e256819b4d7a565f3a973864fdc8e4225e8a3338605aab18d6c1e7099129671fb479a8ee38aa67dde383f012741fcec02afbc82a0f903825820e8c03a03c0b2ddbea4195caf39f41e669f7d251ecf221fbb2f275c0a5d7e05d158402086186c201f064e685ca7fd50f4b313fc1f86b0e0a68cc8db2e4548cb42e2e47ffbdf07c80352484a06332f4e9a180ff8846d3cadd92d0b6717a57482127a08f5f6"
                |> Transaction.deserialize
                |> Expect.notEqual Nothing



-- Helpers


newTxBody : TransactionBody
newTxBody =
    Tx.newBody


newTxWitnessSet : WitnessSet
newTxWitnessSet =
    Tx.newWitnessSet


{-| Convert the internal representation of Natural, using a base 2^26, back into a Natural.
-}
bigNat : List Int -> Natural
bigNat xs =
    let
        step x ( n, base ) =
            ( N.add n (N.mul base (N.fromSafeInt x))
              -- base * 2**26
            , N.mul base (N.fromSafeInt 0x04000000)
            )
    in
    List.foldl step ( N.zero, N.one ) xs
        |> Tuple.first


bytesMap : Dict String v -> BytesMap k v
bytesMap keyValues =
    Dict.toList keyValues
        |> List.map (\( k, v ) -> ( Bytes.fromStringUnchecked k, v ))
        |> Bytes.Map.fromList



-- decodeAnyAndFailTest : Bytes a -> Expectation
-- decodeAnyAndFailTest bytes =
--     Cbor.Decode.decode Cbor.Decode.any (Bytes.toBytes bytes)
--         |> Expect.equal Nothing
