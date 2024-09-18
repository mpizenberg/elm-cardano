module Cardano.Transaction.Builder exposing
    ( Tx, new
    , input, inputData, redeemer, scriptDataHash
    , payToAddress, payToContract, output
    , fee, collateral, collateralReturn, totalCollateral
    , requiredSigner
    , complete
    , toString, newBody, newWitnessSet, referenceInput
    )

{-| Temporary helper module to build transactions.

The end goal is to rewrite a safe Tx building using phantom types exentensible records.
This will enable state machine checks of the building steps at compile time,
to prevent some easy mistakes.

@docs Tx, new

@docs input, inputData, redeemer, scriptDataHash

@docs payToAddress, payToContract, output

@docs fee, collateral, collateralReturn, totalCollateral

@docs requiredSigner

@docs complete

@docs toString, newBody, newWitnessSet, referenceInput

-}

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
import Cardano.Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value as Value
import Natural exposing (Natural)



-- TODO: Deprecate / Remove this module


{-| Temporary helper type to build transactions.
-}
type Tx
    = Tx Transaction


{-| Default transaction body.
-}
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
    , votingProcedures = []
    , proposalProcedures = []
    , currentTreasuryValue = Nothing
    , treasuryDonation = Nothing
    }


{-| Default witness set.
-}
newWitnessSet : WitnessSet
newWitnessSet =
    { vkeywitness = Nothing
    , nativeScripts = Nothing
    , bootstrapWitness = Nothing
    , plutusV1Script = Nothing
    , plutusData = Nothing
    , redeemer = Nothing
    , plutusV2Script = Nothing
    , plutusV3Script = Nothing
    }


{-| Initialize the transaction builder.
-}
new : Tx
new =
    Tx
        { body = newBody
        , witnessSet = newWitnessSet
        , isValid = True
        , auxiliaryData = Nothing
        }


{-| Debugging.
-}
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


{-| Add an input to the Tx.
-}
input : OutputReference -> Tx -> Tx
input newInput (Tx inner) =
    inner |> updateBody (addInput newInput)


{-| Add input data to the witness set.
-}
inputData : Data -> Tx -> Tx
inputData data (Tx inner) =
    inner |> updateWitnessSet (addInputData data)


addInputData : Data -> WitnessSet -> WitnessSet
addInputData data witnessSet =
    { witnessSet
        | plutusData = prependMaybeList data witnessSet.plutusData
    }


{-| Add a redeemer to the witness set.
-}
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


{-| Send Ada and datum to a contract address.
-}
payToContract : Address -> Natural -> Data -> Tx -> Tx
payToContract address amount datum tx =
    tx
        |> output
            { address = address
            , amount = Value.onlyLovelace amount
            , datumOption = Just (DatumValue datum)
            , referenceScript = Nothing
            }


{-| Send Ada to an address.
-}
payToAddress : Address -> Natural -> Tx -> Tx
payToAddress address amount tx =
    tx
        |> output
            { address = address
            , amount = Value.onlyLovelace amount
            , datumOption = Nothing
            , referenceScript = Nothing
            }


{-| Add an output to the Tx.
-}
output : Output -> Tx -> Tx
output newOutput (Tx inner) =
    inner |> updateBody (addOutput newOutput)


addOutput : Output -> TransactionBody -> TransactionBody
addOutput newOutput body =
    { body | outputs = body.outputs ++ [ newOutput ] }


{-| Add fees to the Tx.
-}
fee : Natural -> Tx -> Tx
fee amount (Tx inner) =
    inner |> updateBody (addFee amount)


addFee : Natural -> TransactionBody -> TransactionBody
addFee amount body =
    { body | fee = Just amount }


{-| Add a script data hash to the Tx.
-}
scriptDataHash : Bytes ScriptDataHash -> Tx -> Tx
scriptDataHash dataHash (Tx inner) =
    inner |> updateBody (addScriptDataHash dataHash)


addScriptDataHash : Bytes ScriptDataHash -> TransactionBody -> TransactionBody
addScriptDataHash dataHash body =
    { body | scriptDataHash = Just dataHash }


{-| Add a collateral to the Tx.
-}
collateral : OutputReference -> Tx -> Tx
collateral newInput (Tx inner) =
    inner |> updateBody (addCollateral newInput)


addCollateral : OutputReference -> TransactionBody -> TransactionBody
addCollateral newInput body =
    { body
        | collateral = newInput :: body.collateral
    }


{-| Add a required signer to the Tx.
-}
requiredSigner : Bytes CredentialHash -> Tx -> Tx
requiredSigner signer (Tx inner) =
    inner |> updateBody (addRequiredSigner signer)


addRequiredSigner : Bytes CredentialHash -> TransactionBody -> TransactionBody
addRequiredSigner signer body =
    { body
        | requiredSigners = signer :: body.requiredSigners
    }


{-| Add a collateral return address to the Tx.
-}
collateralReturn : Address -> Natural -> Tx -> Tx
collateralReturn address amount (Tx inner) =
    inner
        |> updateBody
            (addCollateralReturn
                { address = address
                , amount = Value.onlyLovelace amount
                , datumOption = Nothing
                , referenceScript = Nothing
                }
            )


addCollateralReturn : Output -> TransactionBody -> TransactionBody
addCollateralReturn return body =
    { body | collateralReturn = Just return }


{-| Add total collateral to the Tx.
-}
totalCollateral : Int -> Tx -> Tx
totalCollateral amount (Tx inner) =
    inner |> updateBody (addTotalCollateral amount)


addTotalCollateral : Int -> TransactionBody -> TransactionBody
addTotalCollateral amount body =
    { body | totalCollateral = Just amount }


{-| Add a reference input to the Tx.
-}
referenceInput : OutputReference -> Tx -> Tx
referenceInput newInput (Tx inner) =
    inner |> updateBody (addReferenceInput newInput)


addReferenceInput : OutputReference -> TransactionBody -> TransactionBody
addReferenceInput newInput body =
    { body
        | referenceInputs = newInput :: body.referenceInputs
    }


{-| Finalize and serialize the Tx.
-}
complete : Tx -> Bytes Transaction
complete (Tx inner) =
    serialize inner


prependMaybeList : a -> Maybe (List a) -> Maybe (List a)
prependMaybeList value list =
    list
        |> Maybe.withDefault []
        |> (::) value
        |> Just
