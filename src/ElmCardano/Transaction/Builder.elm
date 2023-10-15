module ElmCardano.Transaction.Builder exposing (Tx, complete, input, new, output)

import Bytes exposing (Bytes)
import ElmCardano.Transaction exposing (Input, Output, Transaction, TransactionBody, toCbor)


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
        , isValid = False
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


output : Output -> Tx -> Tx
output newOutput (Tx inner) =
    inner |> updateBody (addOutput newOutput)


addOutput : Output -> TransactionBody -> TransactionBody
addOutput newOutput body =
    { body | outputs = body.outputs ++ [ newOutput ] }


complete : Tx -> Bytes
complete (Tx inner) =
    inner |> toCbor