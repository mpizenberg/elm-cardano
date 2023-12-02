module ElmCardano.TransactionTests exposing (suite)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cbor.Decode
import ElmCardano.Address as Address exposing (NetworkId(..))
import ElmCardano.Data exposing (Data(..))
import ElmCardano.Redeemer exposing (RedeemerTag(..))
import ElmCardano.Transaction as Transaction exposing (TransactionBody)
import ElmCardano.Transaction.Builder as Tx
import ElmCardano.Utxo as Utxo
import ElmCardano.Value as Value
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Tests exposing (expectBytes)


suite : Test
suite =
    describe "ElmCardano.Transaction"
        [ describe "Transaction.toCbor"
            [ test "basic encode" <|
                \_ ->
                    let
                        transactionId =
                            Bytes.fromStringUnchecked "9D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6"

                        -- "70589144cc521615315237f12698f063220efa4bc2f315b6c6e718a6d5"
                        contractAddress =
                            Address.script Testnet (Bytes.fromStringUnchecked "589144cc521615315237f12698f063220efa4bc2f315b6c6e718a6d5")

                        paymentCredential =
                            Bytes.fromStringUnchecked "dd4edd90a2299da2525053c5e18e7c72625f7cf926f5731139d93bae"

                        -- "60dd4edd90a2299da2525053c5e18e7c72625f7cf926f5731139d93bae"
                        userAddress =
                            Address.enterprise Testnet paymentCredential
                    in
                    Tx.new
                        |> Tx.input { transactionId = transactionId, outputIndex = 1 }
                        |> Tx.input { transactionId = transactionId, outputIndex = 0 }
                        |> Tx.inputData (Constr 0 [])
                        |> Tx.redeemer
                            { tag = Spend
                            , index = 0
                            , data = Constr 0 []
                            , exUnits = { mem = 49435, steps = 18305237 }
                            }
                        |> Tx.payToContract contractAddress 50000000 (Constr 0 [ Bytes paymentCredential ])
                        |> Tx.payToAddress userAddress 1947597502
                        |> Tx.fee 182302
                        |> Tx.scriptDataHash (Bytes.fromStringUnchecked "f90cf11d0959b9af8e6fce107acd7a196c21fa3a0d9f1470a8cdec905dcc6d85")
                        |> Tx.collateral { transactionId = transactionId, outputIndex = 1 }
                        |> Tx.requiredSigner paymentCredential
                        |> Tx.collateralReturn userAddress 1897506351
                        |> Tx.totalCollateral 273453
                        |> Tx.referenceInput { transactionId = Bytes.fromStringUnchecked "517b059959fc8ee584689f71cf1d9bb94fc36802aec0faa7fd96182c0ab090c4", outputIndex = 0 }
                        |> Tx.complete
                        |> expectBytes "84A900828258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6008258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6010182A300581D70589144CC521615315237F12698F063220EFA4BC2F315B6C6E718A6D5011A02FAF080028201D8185822D8799F581CDD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAEFF82581D60DD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1A7415FABE021A0002C81E0B5820F90CF11D0959B9AF8E6FCE107ACD7A196C21FA3A0D9F1470A8CDEC905DCC6D850D818258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6010E81581CDD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1082581D60DD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1A7119A62F111A00042C2D1281825820517B059959FC8EE584689F71CF1D9BB94FC36802AEC0FAA7FD96182C0AB090C400A2049FD87980FF0581840000D879808219C11B1A011750D5F5F6"
            ]
        , describe "deserialize"
            [ decode79acf081
            ]
        ]



-- Shelley transactions


decode79acf081 : Test
decode79acf081 =
    test "Tx id 79acf08126546b68d0464417af9530473b8c56c63b2a937bf6451e96e55cb96a" <|
        \_ ->
            Bytes.fromStringUnchecked "83a40085825820397eb970e7980e6ac1eb17fcb26a8df162db4e101f776138d74bbd09ad1a9dee008258207898478682dc3f692bc9bd28cb0df0eaac09a695e17dd563e4d1cc2ad2dc034e008258208db10dfb1c7c68f12c0d7159a39334cad649085e312b5b58ee4dcdad8e0f8fb00082582098fb88bc08d29243d7d10359182a901fdad8a8383bcf52e52af5614787ad394a00825820c1f3b0e99710ada0e479d6f75d60699ce08257b74a18ac170ed86f8dae82911c000185825839019566a8f301fb8a046e44557bb38dfb9080a1213f17f200dcd380816949f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b1b0000005dd4a8a70782583901e54a5d5488b0cfc55a85a806ad5338bd945c500e5f9b1913c0fc5d4149f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b1b0000000945c8cb00825839010ae8c86b7d82139749fc39cbe0ed8756de9285970899d0019db0990f49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b1a7732df0a82583901211c082781577c6b8a4832d29011baab323947e59fbd6ec8995b6c5a49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b1a000f4240825839016a39fc5175611d03cea47f580f465275bf4fc322b61987e83b0b977849f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b1b000000174876e800021a000356f9031a0044aa70a10283845820f202012360fa94af83651a8b8b9592bcda2bee5e187c40d4263a838107c27ae858401c19005d3966c6d3734bde29ec60db8a493c89cb37a2bcbd569efaadd623b28f396de340f40f08b4660dfa728d0cc6ccab627324ad854f1b6b2d16854f1eab095820a1bbc30cf781c0a81b1afc059b7362111f70c45409ca71fc9e165a78e9c978965822a101581e581c8d9b2a782a9b394eff8857682bcd29b048835b93f9a2edf33ede73dd84582054e11b8cc512ce2b45b1b053e8f0e87ba9157a796f5653c20ddfef5af89a18e458406e2b18dde52deabb476ae171ec56b3620c63c0d6a00069f3bd7160ca0af7dd923cbf1ff1b414cb88d824b843b97fce5df70b477fad1fafd62aa1a2556e13f207582052fbfa9e0aca2281d1ef0359f9880b381e8b045273c59c9827f9762e939e768c5822a101581e581c8d9b2a782a9b390866febb68228dd65728c218e7fd86142c4d2736b3845820f5c382061f453618c6c957bb5345860b2736fee4fc29133215134fd92a26d03f5840d26bb37ef4efc404c59865b4f76d51d76c2c3d659430e74f7566884d1cffe5061da326be3aaf54dfddb9e89d69e31289dbee63dabbc5a71ea279bf7b10f8300758203ab2c5f5262ff16de9fe44821853b266d599906090662c40785355931d965f0c5822a101581e581c8d9b2a782a9b392ef1c4a1683cff72b7b6a02cd5dca8afb5c1a15973f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody79acf081
                        , witnessSet = txWitnessSet79acf081
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody79acf081 : TransactionBody
txBody79acf081 =
    { newTxBody
        | inputs =
            [ { transactionId = Bytes.fromStringUnchecked "397eb970e7980e6ac1eb17fcb26a8df162db4e101f776138d74bbd09ad1a9dee"
              , outputIndex = 0
              }
            , { transactionId = Bytes.fromStringUnchecked "7898478682dc3f692bc9bd28cb0df0eaac09a695e17dd563e4d1cc2ad2dc034e"
              , outputIndex = 0
              }
            , { transactionId = Bytes.fromStringUnchecked "8db10dfb1c7c68f12c0d7159a39334cad649085e312b5b58ee4dcdad8e0f8fb0"
              , outputIndex = 0
              }
            , { transactionId = Bytes.fromStringUnchecked "98fb88bc08d29243d7d10359182a901fdad8a8383bcf52e52af5614787ad394a"
              , outputIndex = 0
              }
            , { transactionId = Bytes.fromStringUnchecked "c1f3b0e99710ada0e479d6f75d60699ce08257b74a18ac170ed86f8dae82911c"
              , outputIndex = 0
              }
            ]
        , outputs =
            [ Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "9566a8f301fb8a046e44557bb38dfb9080a1213f17f200dcd3808169")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
                , amount = Value.onlyLovelace 402999781127
                , datumHash = Nothing
                }
            , Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "e54a5d5488b0cfc55a85a806ad5338bd945c500e5f9b1913c0fc5d41")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
                , amount = Value.onlyLovelace 39825492736
                , datumHash = Nothing
                }
            , Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "0ae8c86b7d82139749fc39cbe0ed8756de9285970899d0019db0990f")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
                , amount = Value.onlyLovelace 1999822602
                , datumHash = Nothing
                }
            , Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "211c082781577c6b8a4832d29011baab323947e59fbd6ec8995b6c5a")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
                , amount = Value.onlyLovelace 1000000
                , datumHash = Nothing
                }
            , Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "6a39fc5175611d03cea47f580f465275bf4fc322b61987e83b0b9778")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
                , amount = Value.onlyLovelace 100000000000
                , datumHash = Nothing
                }
            ]
        , fee = Just 218873
        , ttl = Just 4500080
    }


txWitnessSet79acf081 : Transaction.WitnessSet
txWitnessSet79acf081 =
    { newTxWitnessSet
        | bootstrapWitness =
            Just
                [ { publicKey = Bytes.fromStringUnchecked "f202012360fa94af83651a8b8b9592bcda2bee5e187c40d4263a838107c27ae8"
                  , signature = Bytes.fromStringUnchecked "1c19005d3966c6d3734bde29ec60db8a493c89cb37a2bcbd569efaadd623b28f396de340f40f08b4660dfa728d0cc6ccab627324ad854f1b6b2d16854f1eab09"
                  , chainCode = Bytes.fromStringUnchecked "a1bbc30cf781c0a81b1afc059b7362111f70c45409ca71fc9e165a78e9c97896"
                  , attributes = Bytes.fromStringUnchecked "a101581e581c8d9b2a782a9b394eff8857682bcd29b048835b93f9a2edf33ede73dd"
                  }
                , { publicKey = Bytes.fromStringUnchecked "54e11b8cc512ce2b45b1b053e8f0e87ba9157a796f5653c20ddfef5af89a18e4"
                  , signature = Bytes.fromStringUnchecked "6e2b18dde52deabb476ae171ec56b3620c63c0d6a00069f3bd7160ca0af7dd923cbf1ff1b414cb88d824b843b97fce5df70b477fad1fafd62aa1a2556e13f207"
                  , chainCode = Bytes.fromStringUnchecked "52fbfa9e0aca2281d1ef0359f9880b381e8b045273c59c9827f9762e939e768c"
                  , attributes = Bytes.fromStringUnchecked "a101581e581c8d9b2a782a9b390866febb68228dd65728c218e7fd86142c4d2736b3"
                  }
                , { publicKey = Bytes.fromStringUnchecked "f5c382061f453618c6c957bb5345860b2736fee4fc29133215134fd92a26d03f"
                  , signature = Bytes.fromStringUnchecked "d26bb37ef4efc404c59865b4f76d51d76c2c3d659430e74f7566884d1cffe5061da326be3aaf54dfddb9e89d69e31289dbee63dabbc5a71ea279bf7b10f83007"
                  , chainCode = Bytes.fromStringUnchecked "3ab2c5f5262ff16de9fe44821853b266d599906090662c40785355931d965f0c"
                  , attributes = Bytes.fromStringUnchecked "a101581e581c8d9b2a782a9b392ef1c4a1683cff72b7b6a02cd5dca8afb5c1a15973"
                  }
                ]
    }



-- Helpers


newTxBody =
    Tx.newBody


newTxWitnessSet =
    Tx.newWitnessSet


decodeAnyAndFailTest : Bytes a -> Expectation
decodeAnyAndFailTest bytes =
    Cbor.Decode.decode Cbor.Decode.any (Bytes.toBytes bytes)
        |> Expect.equal Nothing
