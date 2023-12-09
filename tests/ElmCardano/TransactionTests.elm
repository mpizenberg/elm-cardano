module ElmCardano.TransactionTests exposing (suite)

import Bytes.Comparable as Bytes
import ElmCardano.Address as Address exposing (NetworkId(..))
import ElmCardano.Data exposing (Data(..))
import ElmCardano.Redeemer exposing (RedeemerTag(..))
import ElmCardano.Transaction as Transaction exposing (TransactionBody, WitnessSet)
import ElmCardano.Transaction.Builder as Tx
import ElmCardano.Utxo as Utxo
import ElmCardano.Value as Value
import Expect
import Natural as N
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
                        |> Tx.payToContract contractAddress (N.fromSafeInt 50000000) (Constr 0 [ Bytes paymentCredential ])
                        |> Tx.payToAddress userAddress (N.fromSafeInt 1947597502)
                        |> Tx.fee (N.fromSafeInt 182302)
                        |> Tx.scriptDataHash (Bytes.fromStringUnchecked "f90cf11d0959b9af8e6fce107acd7a196c21fa3a0d9f1470a8cdec905dcc6d85")
                        |> Tx.collateral { transactionId = transactionId, outputIndex = 1 }
                        |> Tx.requiredSigner paymentCredential
                        |> Tx.collateralReturn userAddress (N.fromSafeInt 1897506351)
                        |> Tx.totalCollateral 273453
                        |> Tx.referenceInput { transactionId = Bytes.fromStringUnchecked "517b059959fc8ee584689f71cf1d9bb94fc36802aec0faa7fd96182c0ab090c4", outputIndex = 0 }
                        |> Tx.complete
                        |> expectBytes "84A900828258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6008258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6010182A300581D70589144CC521615315237F12698F063220EFA4BC2F315B6C6E718A6D5011A02FAF080028201D8185822D8799F581CDD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAEFF82581D60DD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1A7415FABE021A0002C81E0B5820F90CF11D0959B9AF8E6FCE107ACD7A196C21FA3A0D9F1470A8CDEC905DCC6D850D818258209D7F457DD62D2062565F794E42F9ECA458D9CFBCA73A7893899D16F02C2B36B6010E81581CDD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1082581D60DD4EDD90A2299DA2525053C5E18E7C72625F7CF926F5731139D93BAE1A7119A62F111A00042C2D1281825820517B059959FC8EE584689F71CF1D9BB94FC36802AEC0FAA7FD96182C0AB090C400A2049FD87980FF0581840000D879808219C11B1A011750D5F5F6"
            ]
        , describe "deserialize"
            -- Shelley transactions
            [ decode79acf081
            , decode871b14fb
            , decodef3a0835d
            , decode841cca81
            , decode896cf8fe
            , decode3dd8be52
            ]
        ]



-- Shelley transactions


{-| First Shelley transaction!

Tx id: 79acf08126546b68d0464417af9530473b8c56c63b2a937bf6451e96e55cb96a
Previous block intersection:

  - slot: 4492880
  - id: 23fd3b638e8f286978681567d52597b73f7567e18719cef2cbd66bba31303d98

-}
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
                , amount = Value.onlyLovelace (N.fromSafeInt 402999781127)
                , datumHash = Nothing
                }
            , Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "e54a5d5488b0cfc55a85a806ad5338bd945c500e5f9b1913c0fc5d41")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
                , amount = Value.onlyLovelace (N.fromSafeInt 39825492736)
                , datumHash = Nothing
                }
            , Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "0ae8c86b7d82139749fc39cbe0ed8756de9285970899d0019db0990f")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
                , amount = Value.onlyLovelace (N.fromSafeInt 1999822602)
                , datumHash = Nothing
                }
            , Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "211c082781577c6b8a4832d29011baab323947e59fbd6ec8995b6c5a")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
                , amount = Value.onlyLovelace (N.fromSafeInt 1000000)
                , datumHash = Nothing
                }
            , Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "6a39fc5175611d03cea47f580f465275bf4fc322b61987e83b0b9778")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "49f14106ef746c2d3597381d1d5d1c65c91e933acd1baef3fc915f0b")))
                        }
                , amount = Value.onlyLovelace (N.fromSafeInt 100000000000)
                , datumHash = Nothing
                }
            ]
        , fee = Just (N.fromSafeInt 218873)
        , ttl = Just (N.fromSafeInt 4500080)
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


{-| Next Shelley failure.

Tx id: 871b14fbe5abb6cacc63f922187c4f10ea9499055a972eb5d3d4e8771af643df
Block height: 4490537
Previous block intersection:

  - slot: 4493320
  - id: c7da3ab6cc9cac8044d391552acbf72ad0b8d70b1db90ea3c2264418acdac436

-}
decode871b14fb : Test
decode871b14fb =
    test "Tx id 871b14fbe5abb6cacc63f922187c4f10ea9499055a972eb5d3d4e8771af643df" <|
        \_ ->
            Bytes.fromStringUnchecked "83a400818258209a822a5601a29f7a880948cf3b6491c24d861df18dbbe6ea2ba293f9878f965f0001828258390184f8618344721d55a4dd743a08e9628aa098c0c056bcc0ae794a992444adc04f00e3f9af407f93763dec952c12b7e9249a5e98ecd7baa9f11b00000068eb1078688258390149d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e54650e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f45988441a000f4240021a00029201031a0044ac28a100818258200607a454923b9bd5fec2897ce7f2b9ca2874ee545d750624084ba0fc9ef06dd558408c5a9d2ab5343b70afdb9a6b6580c1aea3dc4132968620ef21fdf14f6fd2a97601d8c2e9cc0d7596560c91ae56e9ddc03bbb5efebb6163c59781fafd28d4580af6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody871b14fb
                        , witnessSet = txWitnessSet871b14fb
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody871b14fb : TransactionBody
txBody871b14fb =
    { newTxBody
        | inputs =
            [ { transactionId = Bytes.fromStringUnchecked "9a822a5601a29f7a880948cf3b6491c24d861df18dbbe6ea2ba293f9878f965f"
              , outputIndex = 0
              }
            ]
        , outputs =
            [ Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "84f8618344721d55a4dd743a08e9628aa098c0c056bcc0ae794a9924")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "44adc04f00e3f9af407f93763dec952c12b7e9249a5e98ecd7baa9f1")))
                        }
                , amount = Value.onlyLovelace (N.fromSafeInt 450620323944)
                , datumHash = Nothing
                }
            , Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "49d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e5465")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844")))
                        }
                , amount = Value.onlyLovelace (N.fromSafeInt 1000000)
                , datumHash = Nothing
                }
            ]
        , fee = Just (N.fromSafeInt 168449)
        , ttl = Just (N.fromSafeInt 4500520)
    }


txWitnessSet871b14fb : Transaction.WitnessSet
txWitnessSet871b14fb =
    { newTxWitnessSet
        | vkeywitness =
            Just
                [ { vkey = Bytes.fromStringUnchecked "0607a454923b9bd5fec2897ce7f2b9ca2874ee545d750624084ba0fc9ef06dd5"
                  , signature = Bytes.fromStringUnchecked "8c5a9d2ab5343b70afdb9a6b6580c1aea3dc4132968620ef21fdf14f6fd2a97601d8c2e9cc0d7596560c91ae56e9ddc03bbb5efebb6163c59781fafd28d4580a"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: f3a0835d9359ed79f8301ba61ff263188c180ffd6dfddaba60a7e31b8366c38e
Block height: 4490544
Previous block intersection:

  - slot: 4493460
  - id: afc9552e4a47d6d1a900516749686457b8c864bc4d61c87345db4a3f7ebeb70c

-}
decodef3a0835d : Test
decodef3a0835d =
    test "Tx id f3a0835d9359ed79f8301ba61ff263188c180ffd6dfddaba60a7e31b8366c38e" <|
        \_ ->
            Bytes.fromStringUnchecked "83a50081825820c0810285e7cffd0ea65851008392d41dd4cdf223d9263ca7a33e28a7e7b410b80001818258390149d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e54650e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f45988441b00000068eaea4873021a0002ba0d031a0044b790048182008200581c0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844a10082825820473f36674fcde1ff195076774decda62f4b0ba860f9fcc0c51d63abee8b1e1285840e52151c3a0e54701b9917c1bd21e0e0b1724a474229809308af9b69f83ee3cd1f1377d88166edfcfc159e2441dc272c60700b1f60452910686b88e020af1ba048258209b718dfb7f2b75ac0a9ab4c9fccaa2befdeecdfde9a0970cbc3f41776ca1939558409d386af0ae47af0521316af8cd7d25984dec0d85d00cd2c9c3571ef48fc26a5db7c9056126a8cc6769e72ecd39a0808f33de8680e5997b5ba4169f7007b5c101f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBodyf3a0835d
                        , witnessSet = txWitnessSetf3a0835d
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBodyf3a0835d : TransactionBody
txBodyf3a0835d =
    { newTxBody
        | inputs =
            [ { transactionId = Bytes.fromStringUnchecked "c0810285e7cffd0ea65851008392d41dd4cdf223d9263ca7a33e28a7e7b410b8"
              , outputIndex = 0
              }
            ]
        , outputs =
            [ Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "49d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e5465")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844")))
                        }
                , amount = Value.onlyLovelace (N.fromSafeInt 450617821299)
                , datumHash = Nothing
                }
            ]
        , fee = Just (N.fromSafeInt 178701)
        , ttl = Just (N.fromSafeInt 4503440)
        , certificates =
            [ Transaction.StakeRegistration { delegator = Address.VKeyHash (Bytes.fromStringUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844") }
            ]
    }


txWitnessSetf3a0835d : Transaction.WitnessSet
txWitnessSetf3a0835d =
    { newTxWitnessSet
        | vkeywitness =
            Just
                [ { vkey = Bytes.fromStringUnchecked "473f36674fcde1ff195076774decda62f4b0ba860f9fcc0c51d63abee8b1e128"
                  , signature = Bytes.fromStringUnchecked "e52151c3a0e54701b9917c1bd21e0e0b1724a474229809308af9b69f83ee3cd1f1377d88166edfcfc159e2441dc272c60700b1f60452910686b88e020af1ba04"
                  }
                , { vkey = Bytes.fromStringUnchecked "9b718dfb7f2b75ac0a9ab4c9fccaa2befdeecdfde9a0970cbc3f41776ca19395"
                  , signature = Bytes.fromStringUnchecked "9d386af0ae47af0521316af8cd7d25984dec0d85d00cd2c9c3571ef48fc26a5db7c9056126a8cc6769e72ecd39a0808f33de8680e5997b5ba4169f7007b5c101"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: 841cca81da918feb9fa7257a34630eac95794be712ed3faae6df64f215ce25f2
Block height: 4490550
Previous block intersection:

  - slot: 4493580
  - id: af4fbe98c50ed01fb0b3f0ed2833fd2a3809f4ca2e367fe71665846e599e96fe

-}
decode841cca81 : Test
decode841cca81 =
    test "Tx id 841cca81da918feb9fa7257a34630eac95794be712ed3faae6df64f215ce25f2" <|
        \_ ->
            Bytes.fromStringUnchecked "83a50081825820f3a0835d9359ed79f8301ba61ff263188c180ffd6dfddaba60a7e31b8366c38e0001818258390149d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e54650e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f45988441b00000068cd19f78e021a0002ebe5031a0044b81c04828a03581c153806dbcd134ddee69a8c5204e38ac80448f62342f8c23cfe4b7edf58200220a5d08adbfe9554b52d7b2993be5892ac3ff340e674a377dea3e22ad1778b1b00000068c61714001a1443fd00d81e820318c8581de10e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f459884481581c0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844818400190bba4436dc1428f682783f68747470733a2f2f7261772e67697468756275736572636f6e74656e742e636f6d2f4f6374616c75732f63617264616e6f2f6d61737465722f702e6a736f6e5820ca7d12decf886e31f5226b5946c62edc81a7e40af95ce7cd6465122e309d562683028200581c0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844581c153806dbcd134ddee69a8c5204e38ac80448f62342f8c23cfe4b7edfa10083825820473f36674fcde1ff195076774decda62f4b0ba860f9fcc0c51d63abee8b1e1285840fabb3bdeb0c845517bfb6af55299f08cb4e41e648a032e193274315a0fe027728162b63c4b78729281cd2d9340c4df2b01bb06b4a4eff5bbe0942852fbd5c10b825820e2e08f93a46e151637138038990d37f6891d4620b668891bb89de56f46ffd48758407d487a26ebfc66501b43b64f386ae31c82ee9b9c3108fe52ed5ddf9473f4f719a367522ee048136c68a7ffdd90ded3cad316f0aa4a0bc49eacab11ea9ef4af028258209b718dfb7f2b75ac0a9ab4c9fccaa2befdeecdfde9a0970cbc3f41776ca1939558400b1bcde4d965a7a756121e2335b3a31c83fc4ef800c2f576764539f384e007ec6af1ae7bfe108c68f813d884e066a43f42be2bd2eb8dba802d100d8335f7db09f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody841cca81
                        , witnessSet = txWitnessSet841cca81
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody841cca81 : TransactionBody
txBody841cca81 =
    { newTxBody
        | inputs =
            [ { transactionId = Bytes.fromStringUnchecked "f3a0835d9359ed79f8301ba61ff263188c180ffd6dfddaba60a7e31b8366c38e"
              , outputIndex = 0
              }
            ]
        , outputs =
            [ Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "49d3b2a1cc633fd909591be7cef70d5fe0b2729620d6dd3aac2e5465")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844")))
                        }
                , amount = Value.onlyLovelace (N.fromSafeInt 450117629838)
                , datumHash = Nothing
                }
            ]
        , fee = Just (N.fromSafeInt 191461)
        , ttl = Just (N.fromSafeInt 4503580)
        , certificates =
            [ Transaction.PoolRegistration
                { operator = Bytes.fromStringUnchecked "153806dbcd134ddee69a8c5204e38ac80448f62342f8c23cfe4b7edf"
                , vrfKeyHash = Bytes.fromStringUnchecked "0220a5d08adbfe9554b52d7b2993be5892ac3ff340e674a377dea3e22ad1778b"
                , pledge = N.fromSafeString "450000000000"
                , cost = N.fromSafeInt 340000000
                , margin = { denominator = 200, numerator = 3 }
                , rewardAccount =
                    { networkId = Mainnet
                    , stakeCredential = Address.VKeyHash (Bytes.fromStringUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844")
                    }
                , poolOwners = [ Bytes.fromStringUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844" ]
                , relays =
                    [ Transaction.SingleHostAddr
                        { ipv4 = Just (Bytes.fromStringUnchecked "36dc1428")
                        , ipv6 = Nothing
                        , port_ = Just 3002
                        }
                    ]
                , poolMetadata =
                    Just
                        { poolMetadataHash = Bytes.fromStringUnchecked "ca7d12decf886e31f5226b5946c62edc81a7e40af95ce7cd6465122e309d5626"
                        , url = "https://raw.githubusercontent.com/Octalus/cardano/master/p.json"
                        }
                }
            , Transaction.StakeDelegation
                { delegator = Address.VKeyHash (Bytes.fromStringUnchecked "0e5b086df87a2a0c5c398b41d413f84176c527da5e5cb641f4598844")
                , poolId = Bytes.fromStringUnchecked "153806dbcd134ddee69a8c5204e38ac80448f62342f8c23cfe4b7edf"
                }
            ]
    }


txWitnessSet841cca81 : WitnessSet
txWitnessSet841cca81 =
    { newTxWitnessSet
        | vkeywitness =
            Just
                [ { vkey = Bytes.fromStringUnchecked "473f36674fcde1ff195076774decda62f4b0ba860f9fcc0c51d63abee8b1e128"
                  , signature = Bytes.fromStringUnchecked "fabb3bdeb0c845517bfb6af55299f08cb4e41e648a032e193274315a0fe027728162b63c4b78729281cd2d9340c4df2b01bb06b4a4eff5bbe0942852fbd5c10b"
                  }
                , { vkey = Bytes.fromStringUnchecked "e2e08f93a46e151637138038990d37f6891d4620b668891bb89de56f46ffd487"
                  , signature = Bytes.fromStringUnchecked "7d487a26ebfc66501b43b64f386ae31c82ee9b9c3108fe52ed5ddf9473f4f719a367522ee048136c68a7ffdd90ded3cad316f0aa4a0bc49eacab11ea9ef4af02"
                  }
                , { vkey = Bytes.fromStringUnchecked "9b718dfb7f2b75ac0a9ab4c9fccaa2befdeecdfde9a0970cbc3f41776ca19395"
                  , signature = Bytes.fromStringUnchecked "0b1bcde4d965a7a756121e2335b3a31c83fc4ef800c2f576764539f384e007ec6af1ae7bfe108c68f813d884e066a43f42be2bd2eb8dba802d100d8335f7db09"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: 896cf8fefad1eaf0fa056ba3adf28bfb26b06d1beed64cf790deb595dcb2687a
Block height: 4491210
Previous block intersection:

  - slot: 4506780
  - id: 13bacb73a31ce721831bd82810ed0bc43b7d2190fcad72d0c53b6cc618763dbd

-}
decode896cf8fe : Test
decode896cf8fe =
    test "Tx id 896cf8fefad1eaf0fa056ba3adf28bfb26b06d1beed64cf790deb595dcb2687a" <|
        \_ ->
            Bytes.fromStringUnchecked "83a50081825820e1d1d61ecd706790ee31a1c07fc87827f7b9738d374ef705d1c8cff8295c8cf0000181825839016a8aba085ef5781bf8ea58c5e92408c0bfba7bcc7ca84da90dffcf90d33cabe9bc7a7646243c03f062881d06744b3d53983823178973b9b01b0000000be2a11012021a0002bb6d031a0044c88404818304581c469fbad36a8a68c5f62f3505ecbe6a461ea262ae20b4de8d39ff08ab18d1a100828258206f0d127f3f5122171fec5f91e85df66bac1314680f703abf5caefddffd55a2855840dbf9707786dc151be98e0c189a879d397d9b0aa5bdaad2ff4831fcf53e04651eb1028e78da8f315680056e5435e8bf48917c5cb545eeb0039e5b57a205609f0b82582085640c4b0cb50c31b797c26a8745fc9c9fea7d90dbc3ae241971c141c43ef59758402d0423835dbd83f2118916ce403b2492027f8a4b10752ab5b59a787081af0a547d5c0d72efc521022fd1371a53f00de87ab3816206ba59009530a6bd4cc40f05f6"
                |> Transaction.deserialize
                |> Expect.equal
                    (Just
                        { body = txBody896cf8fe
                        , witnessSet = txWitnessSet896cf8fe
                        , isValid = True
                        , auxiliaryData = Nothing
                        }
                    )


txBody896cf8fe : TransactionBody
txBody896cf8fe =
    { newTxBody
        | inputs =
            [ { transactionId = Bytes.fromStringUnchecked "e1d1d61ecd706790ee31a1c07fc87827f7b9738d374ef705d1c8cff8295c8cf0"
              , outputIndex = 0
              }
            ]
        , outputs =
            [ Utxo.Legacy
                { address =
                    Address.Shelley
                        { networkId = Mainnet
                        , paymentCredential = Address.VKeyHash (Bytes.fromStringUnchecked "6a8aba085ef5781bf8ea58c5e92408c0bfba7bcc7ca84da90dffcf90")
                        , stakeCredential = Just (Address.InlineCredential (Address.VKeyHash (Bytes.fromStringUnchecked "d33cabe9bc7a7646243c03f062881d06744b3d53983823178973b9b0")))
                        }
                , amount = Value.onlyLovelace (N.fromSafeInt 51046846482)
                , datumHash = Nothing
                }
            ]
        , fee = Just (N.fromSafeInt 179053)
        , ttl = Just (N.fromSafeInt 4507780)
        , certificates =
            [ Transaction.PoolRetirement
                { poolId = Bytes.fromStringUnchecked "469fbad36a8a68c5f62f3505ecbe6a461ea262ae20b4de8d39ff08ab"
                , epoch = N.fromSafeInt 209
                }
            ]
    }


txWitnessSet896cf8fe : WitnessSet
txWitnessSet896cf8fe =
    { newTxWitnessSet
        | vkeywitness =
            Just
                [ { vkey = Bytes.fromStringUnchecked "6f0d127f3f5122171fec5f91e85df66bac1314680f703abf5caefddffd55a285"
                  , signature = Bytes.fromStringUnchecked "dbf9707786dc151be98e0c189a879d397d9b0aa5bdaad2ff4831fcf53e04651eb1028e78da8f315680056e5435e8bf48917c5cb545eeb0039e5b57a205609f0b"
                  }
                , { vkey = Bytes.fromStringUnchecked "85640c4b0cb50c31b797c26a8745fc9c9fea7d90dbc3ae241971c141c43ef597"
                  , signature = Bytes.fromStringUnchecked "2d0423835dbd83f2118916ce403b2492027f8a4b10752ab5b59a787081af0a547d5c0d72efc521022fd1371a53f00de87ab3816206ba59009530a6bd4cc40f05"
                  }
                ]
    }


{-| Next Shelley failure.

Tx id: 3dd8be52247b1cfcdc9cd07ef994aabe5c5455c649b0ce28ad101f1c940df0a6
Block height: 4491354
Previous block intersection:

  - slot: 4509660
  - id: c672e5c15afe9cdcb9035e5ca581c263080ba88d689ca8adfbda13503d59a9e2

-}
decode3dd8be52 : Test
decode3dd8be52 =
    test "Tx id 3dd8be52247b1cfcdc9cd07ef994aabe5c5455c649b0ce28ad101f1c940df0a6" <|
        \_ ->
            Bytes.fromStringUnchecked "83a500818258202f744483e15d31c9a3e7407970bb01f9a4542a6cf3ab6d921477cadbc865aa670101818258390158340a42cb6d376ab4b9528767f234330cfb529d7c58dace948bb83988fdf9622c4619c0109132410e76c272013ef07f786e8f2cc24379601b000000746a86b675021a00036b15031a0044f6d804848a03581c3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d458208655a2c115f703f5ffe2eeed634e7f0347c73c90ffcc359d2df7940d526579381b10a741a4627800001a1443fd00d81e820718c8581de1bc000424731d5cfdb41da8bbb2e8bfbdcf05becd055a3f831a0ccdfa83581c09799da84e15b9a32da2bb284586f32fe357afdccecd2fcb8f88dbb5581c88fdf9622c4619c0109132410e76c272013ef07f786e8f2cc2437960581cd1290c017327065ac2279e989735990efac7539823eab4370efde3238284001915294433514501f684001915294433514502f682783468747470733a2f2f756e697465647374616b65736f6663617264616e6f2e696f2f75736130312e6d657461646174612e6a736f6e5820d6bed5fb3aea9ce8b2829859704728ef2987da4b2be7ce07607799a3485ed94983028200581cd1290c017327065ac2279e989735990efac7539823eab4370efde323581c3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d483028200581c09799da84e15b9a32da2bb284586f32fe357afdccecd2fcb8f88dbb5581c3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d483028200581c88fdf9622c4619c0109132410e76c272013ef07f786e8f2cc2437960581c3dc5218de6bf5a29ad6203d9da31b7128c414872e53b8f09767764d4a10086825820643cb214fcfd8886e744e0fdf2ccefe89001c83366ba077394c6d4930158577858406a368d10744c8ca8d84e3f664a1e0aac6b3c5e53bd6b32f5fa9490213310eb6b5d4c5d9487cd6705dc11b44558a3df9d0538a6f8fc05858c9fa40d48d8dc560a8258200df5d54d4e7e95c2ff9b9a3b24b93b8580a1716a522169edf3e07cf0b0e77f6758409f99ffce567794e750e61f410497e6de4d2e7a0fd8e5a8bf3ef5ad6f4b0f1210260220dcedc1443bc48255a72058007d9a4215718f06c73cee914330e1c28f0a825820ddbe1fe594b884d6fb3fe420927a982ed6d5707fb43b59eee0111e0368ffc6ff58400fdc80b27449d5a3e4f9e78c893bce9b10e0fa8c64e70211af77c6df8661b708066d108f28117e1ec3e37d594ebacf4c4c73b1e2dcbdb85b7dd1fec4e75c6605825820e6edd6de6fd75832cdd7429c4eeee5d1fbac17f4306d5b0466d5b4459d0b7e645840f15f4dcf625891e883b714b066aa2b8cbd8a050133b2f0b50164ed928cbf127420ccfd2f59eb32e7ba5a34d65b165807e87cf7f09bfafc4c4d388ed8163f1e08825820c9fbdb7aed0d908da7e3a2d479743233a21b0fdc3b40028ff01c6a590f85a79c58404559a292949757c09daca5eb6dcab3e4e077906fc556445977cd5f5fafb4ef77c64c642a2bd3aefdb5fc2fb6c0249a1b999cc681acdd77ce25627f2850656c01825820d83ff6f61e037f091ab2ce1d5fd925c06f2ea7b0e2d10802ef09c948923650d758408700a2b72d80d6be994a54fc54bc4f212e76a3572b0e6bf8b7b1c1efd99bf348c6a628160b2fcd0c0c7d3e6fa6c6b27a3d2123d9e33b0036abd9118536787b02f6"
                |> Transaction.deserialize
                |> Expect.notEqual Nothing



-- Helpers


newTxBody : TransactionBody
newTxBody =
    Tx.newBody


newTxWitnessSet : WitnessSet
newTxWitnessSet =
    Tx.newWitnessSet



-- decodeAnyAndFailTest : Bytes a -> Expectation
-- decodeAnyAndFailTest bytes =
--     Cbor.Decode.decode Cbor.Decode.any (Bytes.toBytes bytes)
--         |> Expect.equal Nothing
