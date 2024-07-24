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
