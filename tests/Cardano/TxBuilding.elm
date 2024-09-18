module Cardano.TxBuilding exposing (suite)

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as Map
import Cardano exposing (Fee(..), ScriptWitness(..), SpendSource(..), TxFinalizationError(..), TxIntent(..), TxOtherInfo(..), WitnessSource(..), finalizeAdvanced)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..), StakeCredential(..))
import Cardano.CoinSelection as CoinSelection exposing (Error(..))
import Cardano.MultiAsset as MultiAsset exposing (MultiAsset)
import Cardano.Redeemer exposing (Redeemer)
import Cardano.Script as Script
import Cardano.Transaction as Transaction exposing (Transaction, newBody, newWitnessSet)
import Cardano.Transaction.AuxiliaryData.Metadatum as Metadatum exposing (Metadatum)
import Cardano.Uplc as Uplc exposing (evalScriptsCosts)
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
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
                [ Spend <| From testAddr.me (Value.onlyLovelace <| ada 1)
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
                        [ Spend <| From testAddr.me (Value.onlyLovelace <| ada 1)
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
                [ Spend <| From testAddr.me (Value.onlyLovelace <| ada 1)
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
                [ Spend <| From testAddr.you (Value.onlyLovelace <| ada 1)
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
                [ Spend <| From testAddr.me threeCatTwoAda
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
                [ Spend <| From testAddr.me threeCatMinAda
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
                , Spend <| From testAddr.me (Value.onlyToken cat.policyId cat.assetName Natural.one)
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

        -- TODO: test with plutus script spending
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
            , txIntents = [ Spend <| From testAddr.me (Value.onlyLovelace <| ada 1) ]
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
                [ Spend <| From testAddr.me (Value.onlyLovelace <| Natural.fromSafeInt 100)
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
                [ Spend <| From testAddr.me (Value.onlyToken cat.policyId cat.assetName Natural.three)
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

                Ok tx ->
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
