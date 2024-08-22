module Cardano.TxBuilding exposing (suite)

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as Map
import Cardano exposing (Fee(..), ScriptWitness(..), SpendSource(..), TxIntent(..), TxOtherInfo(..), WitnessSource(..), finalize)
import Cardano.Address as Address exposing (Address, Credential(..), NetworkId(..), StakeCredential(..))
import Cardano.CoinSelection as CoinSelection
import Cardano.MultiAsset as MultiAsset exposing (MultiAsset)
import Cardano.Transaction as Transaction exposing (Transaction, newBody, newWitnessSet)
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
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents = []
            }
            (\_ ->
                { newTx
                    | body =
                        { newBody
                            | fee = Just <| Natural.fromSafeInt 2000000
                            , inputs = [ makeRef "0" 0 ]
                        }
                }
            )
        , okTxTest "with just auto fees"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 2 ]
            , fee = autoFee
            , txOtherInfo = []
            , txIntents = []
            }
            (\tx ->
                let
                    feeAmount =
                        Transaction.computeFees tx

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
                }
            )
        , okTxTest "with spending from, and sending to the same address"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
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
                }
            )
        , okTxTest "send 1 ada from me to you"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 5 ]
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
                }
            )
        , okTxTest "mint 1 dog and burn 1 cat"
            { localStateUtxos =
                [ makeAdaOutput 0 testAddr.me 5
                , makeAsset 1 testAddr.me cat.policyIdStr cat.assetNameStr 3
                ]
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
                }
            )
        ]


okTxTest :
    String
    ->
        { localStateUtxos : List ( OutputReference, Output )
        , fee : Fee
        , txOtherInfo : List TxOtherInfo
        , txIntents : List TxIntent
        }
    -> (Transaction -> Transaction)
    -> Test
okTxTest description { localStateUtxos, fee, txOtherInfo, txIntents } expectTransaction =
    test description <|
        \_ ->
            let
                buildingConfig =
                    { localStateUtxos = Utxo.refDictFromList localStateUtxos --   2 ada at my address
                    , coinSelectionAlgo = CoinSelection.largestFirst
                    }
            in
            case finalize buildingConfig fee txOtherInfo txIntents of
                Err error ->
                    Expect.fail error

                Ok tx ->
                    Expect.equal tx <| expectTransaction tx


failTxBuilding : Test
failTxBuilding =
    describe "Detected failure"
        [ failTxTest "when there is no utxo in local state"
            { localStateUtxos = [ makeAdaOutput 0 testAddr.me 2 ]
            , fee = twoAdaFee
            , txOtherInfo = []
            , txIntents = []
            }
            (\error -> Expect.pass)
        ]


failTxTest :
    String
    ->
        { localStateUtxos : List ( OutputReference, Output )
        , fee : Fee
        , txOtherInfo : List TxOtherInfo
        , txIntents : List TxIntent
        }
    -> (String -> Expectation)
    -> Test
failTxTest description { localStateUtxos, fee, txOtherInfo, txIntents } expectedFailure =
    test description <|
        \_ ->
            let
                buildingConfig =
                    { localStateUtxos = Utxo.refDictFromList localStateUtxos --   2 ada at my address
                    , coinSelectionAlgo = CoinSelection.largestFirst
                    }
            in
            case finalize buildingConfig fee txOtherInfo txIntents of
                Err error ->
                    expectedFailure error

                Ok tx ->
                    Expect.fail "This Tx building was not supposed to succeed"


newTx =
    Transaction.new



-- Test data


configGlobalLargest =
    { localStateUtxos = globalStateUtxos
    , coinSelectionAlgo = CoinSelection.largestFirst
    }


globalStateUtxos : Utxo.RefDict Output
globalStateUtxos =
    Utxo.refDictFromList
        [ makeAdaOutput 0 testAddr.me 2 --   2 ada at my address
        , makeAdaOutput 1 testAddr.me 10 -- 10 ada at my address
        , makeAdaOutput 2 testAddr.me 5 --   5 ada at my address
        , makeAsset 3 testAddr.me dog.policyIdStr dog.assetNameStr 2
        , makeAsset 4 testAddr.me cat.policyIdStr cat.assetNameStr 5
        ]


ada : Int -> Natural
ada n =
    Natural.fromSafeInt n
        |> Natural.mul (Natural.fromSafeInt 1000000)


testAddr =
    { me = makeWalletAddress "me"
    , you = makeWalletAddress "you"
    }


dog =
    { scriptRef = makeRef "dogScriptRef" 0
    , policyId = Bytes.fromText "dog"
    , policyIdStr = "dog"
    , assetName = Bytes.fromText "yksoh"
    , assetNameStr = "yksoh"
    }


cat =
    { scriptRef = makeRef "catScriptRef" 0
    , policyId = Bytes.fromText "cat"
    , policyIdStr = "cat"
    , assetName = Bytes.fromText "felix"
    , assetNameStr = "felix"
    }



-- Fee


twoAdaFee =
    ManualFee [ { paymentSource = testAddr.me, exactFeeAmount = Natural.fromSafeInt 2000000 } ]


autoFee =
    AutoFee { paymentSource = testAddr.me }



-- Helper functions


makeWalletAddress : String -> Address
makeWalletAddress name =
    Address.Shelley
        { networkId = Mainnet
        , paymentCredential = VKeyHash (Bytes.fromText name)
        , stakeCredential = Just (InlineCredential (VKeyHash <| Bytes.fromText name))
        }


makeAddress : String -> Address
makeAddress name =
    Bytes.fromText ("key:" ++ name)
        |> Address.enterprise Mainnet


makeRef : String -> Int -> OutputReference
makeRef id index =
    { transactionId = Bytes.fromText id
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
    , Utxo.fromLovelace address (Natural.fromSafeInt <| 1000000 * amount)
    )


makeToken : String -> String -> Int -> Value
makeToken policyId name amount =
    Value.onlyToken (Bytes.fromText policyId) (Bytes.fromText name) (Natural.fromSafeInt amount)
