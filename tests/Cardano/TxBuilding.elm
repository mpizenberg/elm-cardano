module Cardano.TxBuilding exposing (suite)

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as Map
import Cardano exposing (Fee(..), ScriptWitness(..), SpendSource(..), TxFinalizationError(..), TxIntent(..), TxOtherInfo(..), WitnessSource(..), finalizeAdvanced)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..), StakeCredential(..))
import Cardano.CoinSelection as CoinSelection exposing (Error(..))
import Cardano.Data as Data
import Cardano.Metadatum as Metadatum
import Cardano.MultiAsset as MultiAsset
import Cardano.Redeemer exposing (Redeemer)
import Cardano.Script as Script exposing (PlutusVersion(..))
import Cardano.Transaction as Transaction exposing (Transaction, newBody, newWitnessSet)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Expect exposing (Expectation)
import Integer
import Natural exposing (Natural)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Cardano Tx building"
        [ okTxBuilding
        , failTxBuilding
        ]


okTxBuilding : Test
okTxBuilding =
    describe "Successfull"
        [ okTxTest "with just manual fees"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 2 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents = []
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = Just (ada 2)
                            , inputs = [ makeRef "0" 0 ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYme", signature = dummyBytes 64 "SIGNATUREme" } ]
                        }
                }
            )
        , okTxTest "with just auto fees"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 2 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = autoFee
            , txOtherInfo = []
            , txIntents = []
            }
            (\tx ->
                let
                    feeAmount =
                        Transaction.computeFees Transaction.defaultTxFeeParams { refScriptBytes = 0 } tx
                            |> (\{ txSizeFee, scriptExecFee, refScriptSizeFee } ->
                                    Natural.add txSizeFee scriptExecFee |> Natural.add refScriptSizeFee
                               )

                    adaLeft =
                        Natural.sub (ada 2) feeAmount
                in
                { newTx
                    | body =
                        { newBody
                            | fee = Just feeAmount
                            , inputs = [ makeRef "0" 0 ]
                            , outputs = [ Utxo.fromLovelace testAddr.me adaLeft ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYme", signature = dummyBytes 64 "SIGNATUREme" } ]
                        }
                }
            )
        , okTxTest "with spending from, and sending to the same address"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me (Value.onlyLovelace <| ada 1)
                , SendTo testAddr.me (Value.onlyLovelace <| ada 1)
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = Just (ada 2)
                            , inputs = [ makeRef "0" 0 ]
                            , outputs = [ Utxo.fromLovelace testAddr.me (ada 3) ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYme", signature = dummyBytes 64 "SIGNATUREme" } ]
                        }
                }
            )
        , test "simple finalization is able to find fee source" <|
            \_ ->
                let
                    localStateUtxos =
                        Utxo.refDictFromList [ makeAdaOutput 0 testAddr.me 5 ]

                    txIntents =
                        [ Spend <| FromWallet testAddr.me (Value.onlyLovelace <| ada 1)
                        , SendTo testAddr.me (Value.onlyLovelace <| ada 1)
                        ]
                in
                Expect.ok (Cardano.finalize localStateUtxos [] txIntents)
        , okTxTest "send 1 ada from me to you"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me (Value.onlyLovelace <| ada 1)
                , SendTo testAddr.you (Value.onlyLovelace <| ada 1)
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = Just (ada 2)
                            , inputs = [ makeRef "0" 0 ]
                            , outputs =
                                [ Utxo.fromLovelace testAddr.you (ada 1)
                                , Utxo.fromLovelace testAddr.me (ada 2)
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYme", signature = dummyBytes 64 "SIGNATUREme" } ]
                        }
                }
            )
        , okTxTest "I pay the fees for your ada transfer to me"
            { localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , makeAdaOutput 1 testAddr.you 7
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.you (Value.onlyLovelace <| ada 1)
                , SendTo testAddr.me (Value.onlyLovelace <| ada 1)
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = Just (ada 2)
                            , inputs = [ makeRef "0" 0, makeRef "1" 1 ]
                            , outputs =
                                [ Utxo.fromLovelace testAddr.you (ada 6)
                                , Utxo.fromLovelace testAddr.me (ada 4)
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness =
                                Just
                                    -- Two keys since I pay the fee, and spend your utxo
                                    [ { vkey = dummyBytes 32 "VKEYme", signature = dummyBytes 64 "SIGNATUREme" }
                                    , { vkey = dummyBytes 32 "VKEYyou", signature = dummyBytes 64 "SIGNATUREyou" }
                                    ]
                        }
                }
            )
        , let
            threeCat =
                Value.onlyToken cat.policyId cat.assetName Natural.three

            threeCatTwoAda =
                { threeCat | lovelace = ada 2 }
          in
          okTxTest "send 3 cat with 2 ada from me to you"
            { localStateUtxos =
                [ ( makeRef "0" 0, Utxo.fromLovelace testAddr.me (ada 5) )
                , ( makeRef "1" 1, Utxo.simpleOutput testAddr.me threeCat )
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me threeCatTwoAda
                , SendTo testAddr.you threeCatTwoAda
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = Just (ada 2)
                            , inputs = [ makeRef "0" 0, makeRef "1" 1 ]
                            , outputs =
                                [ Utxo.simpleOutput testAddr.you threeCatTwoAda
                                , Utxo.fromLovelace testAddr.me (ada 1)
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYme", signature = dummyBytes 64 "SIGNATUREme" } ]
                        }
                }
            )
        , let
            threeCat =
                Value.onlyToken cat.policyId cat.assetName Natural.three

            minAda =
                Utxo.minAdaForAssets testAddr.you threeCat.assets

            threeCatMinAda =
                { threeCat | lovelace = minAda }
          in
          okTxTest "send 3 cat with minAda from me to you"
            { localStateUtxos =
                [ ( makeRef "0" 0, Utxo.fromLovelace testAddr.me (ada 5) )
                , ( makeRef "1" 1, Utxo.simpleOutput testAddr.me threeCat )
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me threeCatMinAda
                , SendTo testAddr.you threeCatMinAda
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = Just (ada 2)
                            , inputs = [ makeRef "0" 0, makeRef "1" 1 ]
                            , outputs =
                                [ Utxo.simpleOutput testAddr.you threeCatMinAda
                                , Utxo.fromLovelace testAddr.me (Natural.sub (ada 3) minAda)
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYme", signature = dummyBytes 64 "SIGNATUREme" } ]
                        }
                }
            )
        , okTxTest "mint 1 dog and burn 1 cat"
            { localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , makeAsset 1 testAddr.me cat.policyIdStr cat.assetNameStr 3
                , ( dog.scriptRef, dog.refOutput )
                , ( cat.scriptRef, cat.refOutput )
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                -- minting 1 dog
                [ MintBurn
                    { policyId = dog.policyId
                    , assets = Map.singleton dog.assetName Integer.one
                    , scriptWitness = NativeWitness (WitnessReference dog.scriptRef)
                    }
                , SendTo testAddr.me (Value.onlyToken dog.policyId dog.assetName Natural.one)

                -- burning 1 cat
                , Spend <| FromWallet testAddr.me (Value.onlyToken cat.policyId cat.assetName Natural.one)
                , MintBurn
                    { policyId = cat.policyId
                    , assets = Map.singleton cat.assetName Integer.negativeOne
                    , scriptWitness = NativeWitness (WitnessReference cat.scriptRef)
                    }
                ]
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = Just (ada 2)
                            , inputs = [ makeRef "0" 0, makeRef "1" 1 ]
                            , referenceInputs = [ cat.scriptRef, dog.scriptRef ]
                            , mint =
                                MultiAsset.mintAdd
                                    (MultiAsset.onlyToken dog.policyId dog.assetName Integer.one)
                                    (MultiAsset.onlyToken cat.policyId cat.assetName Integer.negativeOne)
                            , outputs =
                                [ { address = testAddr.me
                                  , amount =
                                        Value.onlyLovelace (ada 3)
                                            -- 1 minted dog
                                            |> Value.add (Value.onlyToken dog.policyId dog.assetName Natural.one)
                                            -- 2 cat left after burning 1 from the utxo with 3 cat
                                            |> Value.add (Value.onlyToken cat.policyId cat.assetName Natural.two)
                                  , datumOption = Nothing
                                  , referenceScript = Nothing
                                  }
                                ]
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYme", signature = dummyBytes 64 "SIGNATUREme" } ]
                        }
                }
            )

        -- Test with plutus script spending
        , let
            utxoBeingSpent =
                makeRef "previouslySentToLock" 0

            findSpendingUtxo inputs =
                case inputs of
                    [] ->
                        0

                    ( id, ref ) :: next ->
                        if ref == utxoBeingSpent then
                            id

                        else
                            findSpendingUtxo next

            ( myKeyCred, myStakeCred ) =
                ( Address.extractPubKeyHash testAddr.me
                    |> Maybe.withDefault (Bytes.fromText "should not fail")
                , Address.extractStakeCredential testAddr.me
                )

            -- Lock script made with Aiken
            lock =
                { scriptBytes = Bytes.fromStringUnchecked "58b501010032323232323225333002323232323253330073370e900118041baa0011323232533300a3370e900018059baa00113322323300100100322533301100114a0264a66601e66e3cdd718098010020a5113300300300130130013758601c601e601e601e601e601e601e601e601e60186ea801cdd7180718061baa00116300d300e002300c001300937540022c6014601600460120026012004600e00260086ea8004526136565734aae7555cf2ab9f5742ae881"

                -- { script = PlutusScript PlutusV3 (Bytes.fromStringUnchecked "58b501010032323232323225333002323232323253330073370e900118041baa0011323232533300a3370e900018059baa00113322323300100100322533301100114a0264a66601e66e3cdd718098010020a5113300300300130130013758601c601e601e601e601e601e601e601e601e60186ea801cdd7180718061baa00116300d300e002300c001300937540022c6014601600460120026012004600e00260086ea8004526136565734aae7555cf2ab9f5742ae881")
                , scriptHash = Bytes.fromStringUnchecked "3ff0b1bb5815347c6f0c05328556d80c1f83ca47ac410d25ffb4a330"
                }

            -- Combining the script hash with our stake credential
            -- to keep the locked ada staked.
            lockScriptAddress =
                Address.Shelley
                    { networkId = Mainnet
                    , paymentCredential = ScriptHash lock.scriptHash
                    , stakeCredential = myStakeCred
                    }

            -- Build a redeemer that contains the index of the spent script input.
            redeemer inputsOutputs =
                List.indexedMap Tuple.pair inputsOutputs.spentInputs
                    |> findSpendingUtxo
                    |> (Data.Int << Integer.fromSafeInt)

            -- Helper function to create an output at the lock script address.
            -- It contains our key credential in the datum.
            makeLockedOutput adaAmount =
                { address = lockScriptAddress
                , amount = adaAmount
                , datumOption = Just (DatumValue (Data.Bytes <| Bytes.toAny myKeyCred))
                , referenceScript = Nothing
                }

            localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , ( utxoBeingSpent, makeLockedOutput <| Value.onlyLovelace <| ada 4 )
                ]
          in
          okTxTest "spend 2 ada from a plutus script holding 4 ada"
            { localStateUtxos = localStateUtxos
            , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                -- Collect 2 ada from the lock script
                [ Spend <|
                    FromPlutusScript
                        { spentInput = utxoBeingSpent
                        , datumWitness = Nothing
                        , plutusScriptWitness =
                            { script = ( PlutusV3, WitnessValue lock.scriptBytes )
                            , redeemerData = redeemer
                            , requiredSigners = [ myKeyCred ]
                            }
                        }
                , SendTo testAddr.me (Value.onlyLovelace <| ada 2)

                -- Return the other 2 ada to the lock script (there was 4 ada initially)
                , SendToOutput (makeLockedOutput <| Value.onlyLovelace <| ada 2)
                ]
            }
            (\tx ->
                { newTx
                    | body =
                        { newBody
                            | fee = Just (ada 2)
                            , inputs = [ makeRef "0" 0, utxoBeingSpent ]
                            , requiredSigners = [ myKeyCred ]
                            , outputs =
                                [ makeLockedOutput <| Value.onlyLovelace <| ada 2
                                , Utxo.fromLovelace testAddr.me (ada 5)
                                ]

                            -- script stuff
                            , scriptDataHash = tx.body.scriptDataHash

                            -- collateral would cost 3 ada for 2 ada fees, so return 5-3=2 ada
                            , collateral = [ makeRef "0" 0 ]
                            , totalCollateral = Just 3000000
                            , collateralReturn = Just (Utxo.fromLovelace testAddr.me (ada 2))
                        }
                    , witnessSet =
                        { newWitnessSet
                            | vkeywitness = Just [ { vkey = dummyBytes 32 "VKEYme", signature = dummyBytes 64 "SIGNATUREme" } ]
                            , plutusV3Script = Just [ lock.scriptBytes ]
                            , redeemer =
                                Uplc.evalScriptsCosts Uplc.defaultVmConfig (Utxo.refDictFromList localStateUtxos) tx
                                    |> Result.toMaybe
                        }
                }
            )
        ]


okTxTest :
    String
    ->
        { localStateUtxos : List ( OutputReference, Output )
        , evalScriptsCosts : Utxo.RefDict Output -> Transaction -> Result String (List Redeemer)
        , fee : Fee
        , txOtherInfo : List TxOtherInfo
        , txIntents : List TxIntent
        }
    -> (Transaction -> Transaction)
    -> Test
okTxTest description { localStateUtxos, evalScriptsCosts, fee, txOtherInfo, txIntents } expectTransaction =
    test description <|
        \_ ->
            let
                buildingConfig =
                    { localStateUtxos = Utxo.refDictFromList localStateUtxos --   2 ada at my address
                    , coinSelectionAlgo = CoinSelection.largestFirst
                    , evalScriptsCosts = evalScriptsCosts
                    }
            in
            case finalizeAdvanced buildingConfig fee txOtherInfo txIntents of
                Err error ->
                    Expect.fail (Debug.toString error)

                Ok tx ->
                    Expect.equal tx <| expectTransaction tx


failTxBuilding : Test
failTxBuilding =
    describe "Detected failure"
        [ test "simple finalization cannot find fee source without enough info in Tx intents" <|
            \_ ->
                let
                    localStateUtxos =
                        Utxo.refDictFromList [ makeAdaOutput 0 testAddr.me 5 ]
                in
                Expect.equal (Err UnableToGuessFeeSource) (Cardano.finalize localStateUtxos [] [])
        , failTxTest "when there is no utxo in local state"
            { localStateUtxos = []
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents = []
            }
            (\error ->
                case error of
                    FailedToPerformCoinSelection (UTxOBalanceInsufficient _) ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when there is insufficient manual fee (0.1 ada here)"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = ManualFee [ { paymentSource = testAddr.me, exactFeeAmount = Natural.fromSafeInt 100000 } ]
            , txOtherInfo = []
            , txIntents = []
            }
            (\error ->
                case error of
                    InsufficientManualFee _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when inputs are missing from local state"
            { localStateUtxos = []
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWalletUtxo (makeRef "0" 0)
                , SendTo testAddr.me (Value.onlyLovelace <| ada 1)
                ]
            }
            (\error ->
                case error of
                    ReferenceOutputsMissingFromLocalState [ ref ] ->
                        Expect.equal ref (makeRef "0" 0)

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when Tx intents are unbalanced (too much spend here)"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents = [ Spend <| FromWallet testAddr.me (Value.onlyLovelace <| ada 1) ]
            }
            (\error ->
                case error of
                    UnbalancedIntents _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when Tx intents are unbalanced (too much send here)"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents = [ SendTo testAddr.me (Value.onlyLovelace <| ada 1) ]
            }
            (\error ->
                case error of
                    UnbalancedIntents _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when there is not enough minAda in created output (100 lovelaces here)"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me (Value.onlyLovelace <| Natural.fromSafeInt 100)
                , SendToOutput (Utxo.fromLovelace testAddr.me <| Natural.fromSafeInt 100)
                ]
            }
            (\error ->
                case error of
                    NotEnoughMinAda _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when we send CNT without Ada"
            { localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , makeAsset 1 testAddr.me cat.policyIdStr cat.assetNameStr 3
                ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents =
                [ Spend <| FromWallet testAddr.me (Value.onlyToken cat.policyId cat.assetName Natural.three)
                , SendTo testAddr.you (Value.onlyToken cat.policyId cat.assetName Natural.three)
                ]
            }
            (\error ->
                case error of
                    NotEnoughMinAda _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when there are duplicated metadata tags (tag 0 here)"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo =
                [ TxMetadata { tag = Natural.zero, metadata = Metadatum.Int Integer.one }
                , TxMetadata { tag = Natural.zero, metadata = Metadatum.Int Integer.two }
                ]
            , txIntents = []
            }
            (\error ->
                case error of
                    DuplicatedMetadataTags 0 ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )
        , failTxTest "when validity range is incorrect (start > end)"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
            , evalScriptsCosts = \_ _ -> Ok []
            , fee = twoAdaFee
            , txOtherInfo = [ TxTimeValidityRange { start = 1, end = Natural.zero } ]
            , txIntents = []
            }
            (\error ->
                case error of
                    IncorrectTimeValidityRange _ ->
                        Expect.pass

                    _ ->
                        Expect.fail ("I didn’t expect this failure: " ++ Debug.toString error)
            )

        -- TODO: test for collateral selection error
        ]


failTxTest :
    String
    ->
        { localStateUtxos : List ( OutputReference, Output )
        , evalScriptsCosts : Utxo.RefDict Output -> Transaction -> Result String (List Redeemer)
        , fee : Fee
        , txOtherInfo : List TxOtherInfo
        , txIntents : List TxIntent
        }
    -> (TxFinalizationError -> Expectation)
    -> Test
failTxTest description { localStateUtxos, evalScriptsCosts, fee, txOtherInfo, txIntents } expectedFailure =
    test description <|
        \_ ->
            let
                buildingConfig =
                    { localStateUtxos = Utxo.refDictFromList localStateUtxos --   2 ada at my address
                    , coinSelectionAlgo = CoinSelection.largestFirst
                    , evalScriptsCosts = evalScriptsCosts
                    }
            in
            case finalizeAdvanced buildingConfig fee txOtherInfo txIntents of
                Err error ->
                    expectedFailure error

                Ok _ ->
                    Expect.fail "This Tx building was not supposed to succeed"


newTx =
    Transaction.new



-- Test data


testAddr =
    { me = makeWalletAddress "me"
    , you = makeWalletAddress "you"
    }


dog =
    { policyId = dummyCredentialHash "dog"
    , policyIdStr = "dog"
    , assetName = Bytes.fromText "yksoh"
    , assetNameStr = "yksoh"
    , scriptRef = makeRef "dogScriptRef" 0
    , refOutput =
        { address = makeAddress "dogScriptRefAddress"
        , amount = Value.onlyLovelace (ada 5)
        , datumOption = Nothing
        , referenceScript = Just <| Script.Native <| Script.ScriptAll [] -- dummy
        }
    }


cat =
    { policyId = dummyCredentialHash "cat"
    , policyIdStr = "cat"
    , assetName = Bytes.fromText "felix"
    , assetNameStr = "felix"
    , scriptRef = makeRef "catScriptRef" 0
    , refOutput =
        { address = makeAddress "catScriptRefAddress"
        , amount = Value.onlyLovelace (ada 6)
        , datumOption = Nothing
        , referenceScript = Just <| Script.Native <| Script.ScriptAll [] -- dummy
        }
    }



-- Fee


twoAdaFee =
    ManualFee [ { paymentSource = testAddr.me, exactFeeAmount = ada 2 } ]


autoFee =
    AutoFee { paymentSource = testAddr.me }



-- Helper functions


{-| Unsafe helper function to make up some bytes of a given length,
starting by the given text when decoded as text.
-}
dummyBytes : Int -> String -> Bytes a
dummyBytes length prefix =
    let
        zeroSuffix =
            String.repeat (length - String.length prefix) "0"
    in
    Bytes.fromText (prefix ++ zeroSuffix)


dummyCredentialHash : String -> Bytes CredentialHash
dummyCredentialHash str =
    dummyBytes 28 str


makeWalletAddress : String -> Address
makeWalletAddress name =
    Address.Shelley
        { networkId = Mainnet
        , paymentCredential = VKeyHash (dummyCredentialHash name)
        , stakeCredential = Just (InlineCredential (VKeyHash <| dummyCredentialHash name))
        }


makeAddress : String -> Address
makeAddress name =
    Address.enterprise Mainnet (dummyCredentialHash name)


makeRef : String -> Int -> OutputReference
makeRef id index =
    { transactionId = dummyBytes 32 id
    , outputIndex = index
    }


makeAsset : Int -> Address -> String -> String -> Int -> ( OutputReference, Output )
makeAsset index address policyId name amount =
    ( makeRef (String.fromInt index) index
    , { address = address
      , amount = makeToken policyId name amount
      , datumOption = Nothing
      , referenceScript = Nothing
      }
    )


makeAdaOutput : Int -> Address -> Int -> ( OutputReference, Output )
makeAdaOutput index address amount =
    ( makeRef (String.fromInt index) index
    , Utxo.fromLovelace address (ada amount)
    )


makeToken : String -> String -> Int -> Value
makeToken policyId name amount =
    Value.onlyToken (dummyCredentialHash policyId) (Bytes.fromText name) (Natural.fromSafeInt amount)


ada : Int -> Natural
ada n =
    Natural.fromSafeInt n
        |> Natural.mul (Natural.fromSafeInt 1000000)
