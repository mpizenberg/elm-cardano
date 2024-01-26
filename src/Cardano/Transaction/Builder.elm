module Cardano.Transaction.Builder exposing
    ( Tx
    , collateral
    , collateralReturn
    , complete
    , fee
    , input
    , inputData
    , new
    , newBody
    , newWitnessSet
    , output
    , payToAddress
    , payToContract
    , redeemer
    , referenceInput
    , requiredSigner
    , scriptDataHash
    , toString
    , totalCollateral
    )

import Bytes.Comparable exposing (Bytes)
import Cardano.Address exposing (Address, CredentialHash)
import Cardano.Data exposing (Data)
import Cardano.MultiAsset as MultiAsset
import Cardano.Redeemer exposing (Redeemer)
import Cardano.Transaction
    exposing
        ( ScriptDataHash
        , Transaction
        , TransactionBody
        , WitnessSet
        , serialize
        )
import Cardano.Utxo exposing (DatumOption(..), Output(..), OutputReference)
import Cardano.Value as Value
import Natural exposing (Natural)


type Tx
    = Tx Transaction


newBody : TransactionBody
newBody =
    { inputs = []
    , outputs = []
    , fee = Nothing
    , ttl = Nothing
    , certificates = []
    , withdrawals = []
    , update = Nothing
    , auxiliaryDataHash = Nothing
    , validityIntervalStart = Nothing
    , mint = MultiAsset.empty
    , scriptDataHash = Nothing
    , collateral = []
    , requiredSigners = []
    , networkId = Nothing
    , collateralReturn = Nothing
    , totalCollateral = Nothing
    , referenceInputs = []
    }


newWitnessSet : WitnessSet
newWitnessSet =
    { vkeywitness = Nothing
    , nativeScripts = Nothing
    , bootstrapWitness = Nothing
    , plutusV1Script = Nothing
    , plutusData = Nothing
    , redeemer = Nothing
    , plutusV2Script = Nothing
    }


new : Tx
new =
    Tx
        { body = newBody
        , witnessSet = newWitnessSet
        , isValid = True
        , auxiliaryData = Nothing
        }


toString : Tx -> String
toString (Tx inner) =
    Debug.toString inner


updateBody : (TransactionBody -> TransactionBody) -> Transaction -> Tx
updateBody apply inner =
    Tx
        { inner
            | body = apply inner.body
        }


updateWitnessSet : (WitnessSet -> WitnessSet) -> Transaction -> Tx
updateWitnessSet apply inner =
    Tx
        { inner
            | witnessSet = apply inner.witnessSet
        }


input : OutputReference -> Tx -> Tx
input newInput (Tx inner) =
    inner |> updateBody (addInput newInput)


inputData : Data -> Tx -> Tx
inputData data (Tx inner) =
    inner |> updateWitnessSet (addInputData data)


addInputData : Data -> WitnessSet -> WitnessSet
addInputData data witnessSet =
    { witnessSet
        | plutusData = prependMaybeList data witnessSet.plutusData
    }


redeemer : Redeemer -> Tx -> Tx
redeemer r (Tx inner) =
    inner |> updateWitnessSet (addRedeemer r)


addRedeemer : Redeemer -> WitnessSet -> WitnessSet
addRedeemer r witnessSet =
    { witnessSet
        | redeemer = prependMaybeList r witnessSet.redeemer
    }


addInput : OutputReference -> TransactionBody -> TransactionBody
addInput newInput body =
    { body | inputs = newInput :: body.inputs }


payToContract : Address -> Natural -> Data -> Tx -> Tx
payToContract address amount datum tx =
    tx
        |> output
            (PostAlonzo
                { address = address
                , value = Value.onlyLovelace amount
                , datumOption = Just (Datum datum)
                , referenceScript = Nothing
                }
            )


payToAddress : Address -> Natural -> Tx -> Tx
payToAddress address amount tx =
    tx
        |> output
            (Legacy
                { address = address
                , amount = Value.onlyLovelace amount
                , datumHash = Nothing
                }
            )


output : Output -> Tx -> Tx
output newOutput (Tx inner) =
    inner |> updateBody (addOutput newOutput)


addOutput : Output -> TransactionBody -> TransactionBody
addOutput newOutput body =
    { body | outputs = body.outputs ++ [ newOutput ] }


fee : Natural -> Tx -> Tx
fee amount (Tx inner) =
    inner |> updateBody (addFee amount)


addFee : Natural -> TransactionBody -> TransactionBody
addFee amount body =
    { body | fee = Just amount }


scriptDataHash : ScriptDataHash -> Tx -> Tx
scriptDataHash dataHash (Tx inner) =
    inner |> updateBody (addScriptDataHash dataHash)


addScriptDataHash : ScriptDataHash -> TransactionBody -> TransactionBody
addScriptDataHash dataHash body =
    { body | scriptDataHash = Just dataHash }


collateral : OutputReference -> Tx -> Tx
collateral newInput (Tx inner) =
    inner |> updateBody (addCollateral newInput)


addCollateral : OutputReference -> TransactionBody -> TransactionBody
addCollateral newInput body =
    { body
        | collateral = newInput :: body.collateral
    }


requiredSigner : CredentialHash -> Tx -> Tx
requiredSigner signer (Tx inner) =
    inner |> updateBody (addRequiredSigner signer)


addRequiredSigner : CredentialHash -> TransactionBody -> TransactionBody
addRequiredSigner signer body =
    { body
        | requiredSigners = signer :: body.requiredSigners
    }


collateralReturn : Address -> Natural -> Tx -> Tx
collateralReturn address amount (Tx inner) =
    inner
        |> updateBody
            (addCollateralReturn
                (Legacy
                    { address = address
                    , amount = Value.onlyLovelace amount
                    , datumHash = Nothing
                    }
                )
            )


addCollateralReturn : Output -> TransactionBody -> TransactionBody
addCollateralReturn return body =
    { body | collateralReturn = Just return }


totalCollateral : Int -> Tx -> Tx
totalCollateral amount (Tx inner) =
    inner |> updateBody (addTotalCollateral amount)


addTotalCollateral : Int -> TransactionBody -> TransactionBody
addTotalCollateral amount body =
    { body | totalCollateral = Just amount }


referenceInput : OutputReference -> Tx -> Tx
referenceInput newInput (Tx inner) =
    inner |> updateBody (addReferenceInput newInput)


addReferenceInput : OutputReference -> TransactionBody -> TransactionBody
addReferenceInput newInput body =
    { body
        | referenceInputs = newInput :: body.referenceInputs
    }


complete : Tx -> Bytes Transaction
complete (Tx inner) =
    serialize inner


prependMaybeList : a -> Maybe (List a) -> Maybe (List a)
prependMaybeList value list =
    list
        |> Maybe.withDefault []
        |> (::) value
        |> Just
