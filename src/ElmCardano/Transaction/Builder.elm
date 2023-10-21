module ElmCardano.Transaction.Builder exposing
    ( Tx
    , collateral
    , collateralReturn
    , complete
    , fee
    , input
    , new
    , output
    , payToAddress
    , payToContract
    , referenceInput
    , requiredSigner
    , scriptDataHash
    , totalCollateral
    )

import Bytes exposing (Bytes)
import ElmCardano.Core exposing (Coin, Data)
import ElmCardano.Transaction exposing (DatumOption(..), Input, Output(..), Transaction, TransactionBody, Value(..), toCbor)


type Tx
    = Tx Transaction


new : Tx
new =
    Tx
        { body =
            { inputs = []
            , outputs = []
            , fee = 0
            , ttl = Nothing
            , certificates = Nothing
            , withdrawals = Nothing
            , update = Nothing
            , auxiliaryDataHash = Nothing
            , validityIntervalStart = Nothing
            , mint = Nothing
            , scriptDataHash = Nothing
            , collateral = Nothing
            , requiredSigners = Nothing
            , networkId = Nothing
            , collateralReturn = Nothing
            , totalCollateral = Nothing
            , referenceInputs = Nothing
            }
        , witnessSet =
            { vkeywitness = Nothing
            , nativeScripts = Nothing
            , bootstrapWitness = Nothing
            , plutusV1Script = Nothing
            , plutusData = Nothing
            , redeemer = Nothing
            , plutusV2Script = Nothing
            }
        , isValid = True
        , auxiliaryData = Nothing
        }


updateBody : (TransactionBody -> TransactionBody) -> Transaction -> Tx
updateBody apply inner =
    Tx
        { inner
            | body = apply inner.body
        }


input : Input -> Tx -> Tx
input newInput (Tx inner) =
    inner |> updateBody (addInput newInput)


addInput : Input -> TransactionBody -> TransactionBody
addInput newInput body =
    { body | inputs = newInput :: body.inputs }


payToContract : Bytes -> Coin -> Data -> Tx -> Tx
payToContract address amount datum tx =
    tx
        |> output
            (PostAlonzo
                { address = address
                , value = Coin amount
                , datumOption = Just (Datum datum)
                , referenceScript = Nothing
                }
            )


payToAddress : Bytes -> Coin -> Tx -> Tx
payToAddress address amount tx =
    tx
        |> output
            (Legacy
                { address = address
                , amount = Coin amount
                , datumHash = Nothing
                }
            )


output : Output -> Tx -> Tx
output newOutput (Tx inner) =
    inner |> updateBody (addOutput newOutput)


addOutput : Output -> TransactionBody -> TransactionBody
addOutput newOutput body =
    { body | outputs = body.outputs ++ [ newOutput ] }


fee : Int -> Tx -> Tx
fee amount (Tx inner) =
    inner |> updateBody (addFee amount)


addFee : Int -> TransactionBody -> TransactionBody
addFee amount body =
    { body | fee = amount }


scriptDataHash : Bytes -> Tx -> Tx
scriptDataHash dataHash (Tx inner) =
    inner |> updateBody (addScriptDataHash dataHash)


addScriptDataHash : Bytes -> TransactionBody -> TransactionBody
addScriptDataHash dataHash body =
    { body | scriptDataHash = Just dataHash }


collateral : Input -> Tx -> Tx
collateral newInput (Tx inner) =
    inner |> updateBody (addCollateral newInput)


addCollateral : Input -> TransactionBody -> TransactionBody
addCollateral newInput body =
    { body
        | collateral =
            body.collateral
                |> Maybe.withDefault []
                |> (::) newInput
                |> Just
    }


requiredSigner : Bytes -> Tx -> Tx
requiredSigner signer (Tx inner) =
    inner |> updateBody (addRequiredSigner signer)


addRequiredSigner : Bytes -> TransactionBody -> TransactionBody
addRequiredSigner signer body =
    { body
        | requiredSigners =
            body.requiredSigners
                |> Maybe.withDefault []
                |> (::) signer
                |> Just
    }


collateralReturn : Bytes -> Coin -> Tx -> Tx
collateralReturn address amount tx =
    tx
        |> output
            (Legacy
                { address = address
                , amount = Coin amount
                , datumHash = Nothing
                }
            )


totalCollateral : Int -> Tx -> Tx
totalCollateral amount (Tx inner) =
    inner |> updateBody (addTotalCollateral amount)


addTotalCollateral : Int -> TransactionBody -> TransactionBody
addTotalCollateral amount body =
    { body | totalCollateral = Just amount }


referenceInput : Input -> Tx -> Tx
referenceInput newInput (Tx inner) =
    inner |> updateBody (addReferenceInput newInput)


addReferenceInput : Input -> TransactionBody -> TransactionBody
addReferenceInput newInput body =
    { body
        | collateral =
            body.referenceInputs
                |> Maybe.withDefault []
                |> (::) newInput
                |> Just
    }


complete : Tx -> Bytes
complete (Tx inner) =
    inner |> toCbor
