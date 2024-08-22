module Cardano.TxBuilding exposing (suite)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (Fee(..), SpendSource(..), TxIntent(..), TxOtherInfo(..), finalize)
import Cardano.Address as Address exposing (Address, Credential(..), NetworkId(..), StakeCredential(..))
import Cardano.CoinSelection as CoinSelection
import Cardano.Transaction as Transaction exposing (newBody, newWitnessSet)
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Expect
import Natural exposing (Natural)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Cardano Tx building"
        [ txWithJustManualFees
        , txWithJustAutoFees
        ]


txWithJustManualFees : Test
txWithJustManualFees =
    test "Tx with just manual fees" <|
        \_ ->
            let
                buildingConfig =
                    { localStateUtxos = Utxo.refDictFromList [ makeAdaOutput 0 testAddr.me 2 ] --   2 ada at my address
                    , coinSelectionAlgo = CoinSelection.largestFirst
                    }
            in
            case finalize buildingConfig twoAdaFee [] [] of
                Err error ->
                    Expect.fail error

                Ok tx ->
                    Expect.equal tx <|
                        { newTx
                            | body =
                                { newBody
                                    | fee = Just <| Natural.fromSafeInt 2000000
                                    , inputs = [ makeRef "0" 0 ]
                                }
                        }


txWithJustAutoFees : Test
txWithJustAutoFees =
    test "Tx with just auto fees" <|
        \_ ->
            let
                buildingConfig =
                    { localStateUtxos = Utxo.refDictFromList [ makeAdaOutput 0 testAddr.me 2 ] --   2 ada at my address
                    , coinSelectionAlgo = CoinSelection.largestFirst
                    }
            in
            case finalize buildingConfig autoFee [] [] of
                Err error ->
                    Expect.fail error

                Ok tx ->
                    let
                        feeAmount =
                            Transaction.computeFees tx

                        adaLeft =
                            Natural.sub (ada 2) feeAmount
                    in
                    Expect.equal tx <|
                        { newTx
                            | body =
                                { newBody
                                    | fee = Just feeAmount
                                    , inputs = [ makeRef "0" 0 ]
                                    , outputs = [ Utxo.fromLovelace testAddr.me adaLeft ]
                                }
                        }


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
