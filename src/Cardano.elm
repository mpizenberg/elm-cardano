module Cardano exposing
    ( Tx, Intent, SourceOwner(..), DestinationOwner, from, to
    , BasicUtxoSelection(..), simpleTransfer, transfer
    , mintAndBurnViaNativeScript, spendFromNativeScript, sendToNativeScript
    , mintAndBurnViaPlutusScript, ScriptUtxoSelection(..), spendFromPlutusScript, sendToPlutusScript, withdrawViaPlutusScript
    , ChangeReallocation, handleChange, changeBackToSource, changeBackTo
    , addMetadata
    , constrainTimeValidity
    , addRequiredSigners
    , setFeesManually
    , LocalState, finalizeTx
    )

{-| Cardano stuff


# Transaction Building Overview

In order to provide elegant transaction building blocks, we must understand what transactions are.
For this, we’ll use a framework composed of 4 points:

1.  Intent: what we want to achieve with this transaction
      - Transfer: send some tokens from somewhere (including rewards) to somewhere else
      - Mint and burn: create and destroy tokens
      - Use a script: provide/spend tokens and data to/from a script
      - Stake management: delegations and pool registrations
2.  Metadata: additional information
3.  Constraints: what additional constraints do we want to set
      - Temporal validity range: first/last slots when the Tx is valid
4.  Requirements: what is imposed by the protocol
      - Tx fee: depends on size/mem/cpu
      - Hashes: for metadata and script data
      - Collateral: for plutus scripts
      - Signatures: for consuming inputs and scripts requirements

This API revolves around composing intents, then adding metadata and constraints,
and finally trying to validate it and auto-populate all requirements.
That’s enough theory, let’s get more concrete.

Let’s first define some addresses we are going to be using.

    me =
        Address.fromAddr "addr..."
            |> Maybe.withDefault shouldNotErrorIfIsACorrectAddress

    you =
        Address.fromAddr "addr..."
            |> Maybe.withDefault shouldNotErrorIfIsACorrectAddress

    someone =
        Address.fromAddr "addr..."
            |> Maybe.withDefault shouldNotErrorIfIsACorrectAddress

    fromMe =
        from me
            |> Maybe.withDefault shouldNotErrorIfIsAnActualVKeyCredential

    fromYou =
        from you
            |> Maybe.withDefault shouldNotErrorIfIsAnActualVKeyCredential

    toSomeone =
        to someone
            |> Maybe.withDefault shouldNotErrorIfIsAnActualVKeyCredential

Here is a simple way to send 1 Ada to someone else.

    localState =
        -- We already previously retrieved a list of our UTxOs
        { utxos = myUtxos }

    oneAda =
        -- Asset amounts are typed with unbounded Natural numbers
        Value.onlyLovelace (Natural.fromSafeString "1000000")

    sendToSomeoneTx =
        simpleTransfer fromMe toSomeone oneAda
            |> finalizeTx Mainnet costModels localState defaultSelectionAlgo
            |> signTx

We need two additional steps after specifying the transfer intention.
The finalization step validates the Tx, compute the fees and add other required fields.
Finally, we need to sign the transaction.

More control on the transfer is possible if we want to have multiple senders and receivers.
Here is an example where me and you both contribute 1 Ada.

    inputs =
        [ { source = fromMe, utxoSelection = AutoUtxoSelection, assets = oneAda }
        , { source = fromYou, utxoSelection = AutoUtxoSelection, assets = oneAda }
        ]

    outputs =
        [ { destination = toSomeone, assets = Value.onlyLovelace (Natural.add oneAda oneAda) } ]

    localState =
        -- We already previously retrieved a list of our UTxOs
        { utxos = myUtxos ++ yourUtxos }

    bothSendToSomeoneTx =
        initTx
            |> transfer inputs outputs
            |> handleChange changeBackToSource
            |> finalizeTx Mainnet costModels localState defaultSelectionAlgo
            |> signTx

As you can see there are two additional steps here compared to the previous example.
An initialization step, and a change handling step.
In cardano, you cannot modify UTxOs.
You have to spend them entirely and decide what to do with the unused change.

The `sendToSomeone` shortcut function does both with some defaults.
This removes a bit of verbosity but isn’t composable with other building blocks.
To use all other building blocks, we need to call ourself these two steps.

To mint or burn via a native script, here is what we can do.

    dogScriptSource =
        ReferencedNativeScript
            { outputRef = dogOutputRef
            , scriptHash = dogPolicyId
            }

    catScriptSource =
        ReferencedNativeScript
            { outputRef = catOutputRef
            , scriptHash = catPolicyId
            }

    autoSelectFromMe assets =
        { source = fromMe, utxoSelection = AutoUtxoSelection, assets = assets }

    backToMe assets =
        { destination = toMe, assets = assets }

    mintDogAndBurnCatTx =
        initTx
            -- minting 1 dog (amounts are of type Integer: unbounded positive or negative integers)
            |> mintAndBurnViaNativeScript dogScriptSource [ { asset = dogAssetName, amount = Integer.one } ]
            -- burning 1 cat
            |> mintAndBurnViaNativeScript catScriptSource [ { asset = catAssetName, amount = Integer.negate Integer.one } ]
            -- balancing the mint and burn
            |> transfer
                [ autoSelectFromMe (Value.onlyToken catPolicyId catAssetName Natural.one) ]
                [ backToMe (Value.onlyToken dogPolicyId dogAssetName Natural.one) ]
            |> handleChange changeBackToSource
            |> finalizeTx Mainnet costModels localState defaultSelectionAlgo
            |> signTx

As you can see, we cannot simply use the mint and burn steps,
and must also add some transfer step to the transaction.
This is because transactions must conserve a balanced ledger.
So in order to be able to validate that your transaction is correct in the finalization step,
we must know what to do with the mint tokens and where are the burned tokens coming from.
We could have added parameters to the mint and burn functions but it would have degraded
Cardano composability capabilities, especially when calling different contracts in the same Tx.

Let’s show how to use a native script to lock some tokens,
that can only be retrieved with our signature.

    lockScript =
        ScriptPubkey myPubkeyHash

    lockScriptHash =
        -- will be provided by the Elm library
        computeNativeScriptHash lockScript

    myStakeCredential =
        toMe.stakeCred

    lockTx =
        initTx
            |> transfer [ autoSelectFromMe twoAda ] []
            |> sendToNativeScript lockScriptHash myStakeCredential twoAda
            |> handleChange changeBackToSource
            |> finalizeTx Mainnet costModels localState defaultSelectionAlgo
            |> signTx

As you can see, we could even keep our stake credential
while locking our ada into the script address,
meaning the locked ada will still be counted in our stake for the rewards.
This is because Cardano addresses have two parts.
The native script logic only affects the first part of the address.

Ok, now let’s show an example how to spend utxos from a native script.
Imagine we have a script where we had locked some ada,
only retrievable with our signature.
Now we want to retrieve 1 ada from it, and keep the other ada locked.

    lockScriptSource =
        EmbeddedNativeScript { script = lockScript, scriptHash = lockScriptHash }

    localState =
        -- We already updated a list of our UTxOs
        { utxos = myUtxos ++ scriptUtxos }

    unlockTx =
        initTx
            |> spendFromNativeScript lockScriptSource AutoUtxoSelection oneAda
            |> addRequiredSigners [ toMe.paymentKey ]
            |> transfer [] [ { destination = toMe, assets = oneAda } ]
            -- changeBackToSource will send the 1 ada change back to the contract address
            |> handleChange changeBackToSource
            |> finalizeTx Mainnet costModels localState defaultSelectionAlgo
            |> signTx

Alright, how about doing all those things with Plutus scripts now?
The main difference with native scripts is that UTxOs sent to Plutus script custody
"should" (and after Chang HF, "can") contain additional metadata, also called "Datum".
This datum is then one input of the Plutus script when trying to spend that UTxO.
In addition to the datum, fixed when sending the UTxO to the script address,
we also need to add a "redeemer" when invoking the Plutus script.
This is another piece of data, always provided for all types of Plutus script invocations.
Ok, let’s start with the minting and burning example.

    dogScriptSource =
        ReferencedPlutusScript
            { outputRef = dogOutputRef
            , scriptHash = dogPolicyId
            }

    dogRedeemer =
        buildRedeemerAccordingToDogScriptBlueprint

    catScriptSource =
        ReferencedPlutusScript
            { outputRef = catOutputRef
            , scriptHash = catPolicyId
            }

    catRedeemer =
        buildRedeemerAccordingToCatScriptBlueprint

    autoSelectFromMe assets =
        { source = fromMe, utxoSelection = AutoUtxoSelection, assets = assets }

    backToMe assets =
        { destination = toMe, assets = assets }

    mintDogAndBurnCatTx =
        initTx
            -- notice we pass a redeemer for each mint/burn, that will be used by the script execution
            |> mintAndBurnViaPlutusScript
                dogScriptSource
                dogRedeemer
                [ { asset = dogAssetName, amount = Integer.one } ]
            |> mintAndBurnViaPlutusScript
                catScriptSource
                catRedeemer
                [ { asset = catAssetName, amount = Integer.negate Integer.one } ]
            -- balancing the mint and burn
            |> transfer
                [ autoSelectFromMe (Value.onlyToken catPolicyId catAssetName Natural.one) ]
                [ backToMe (Value.onlyToken dogPolicyId dogAssetName Natural.one) ]
            |> handleChange changeBackToSource
            |> finalizeTx Mainnet costModels localState defaultSelectionAlgo
            |> signTx

Ok now let’s show how sending to a Plutus script would be done.
As before, we’ll use the simple example of a lock script.
But this time, we don’t write it directly (as in the NativeScript example),
instead we suppose the contract was written in another language (Aiken, plu-ts, Opshin, ...).
But the blueprint of the contract, with its hash is available.

    lockScriptHash =
        extractedFromBlueprint

    -- why not keep it staked while we are at it :)
    myStakeCredential =
        toMe.stakeCred

    -- put the unlocking pubkey in the datum of the funds we lock
    datum =
        Data.Bytes (Bytes.toAny toMe.paymentKey)

    lockTx =
        initTx
            |> transfer [ autoSelectFromMe twoAda ] []
            -- notice the "datum" that’s new here
            |> sendToPlutusScript lockScriptHash myStakeCredential datum twoAda
            |> handleChange changeBackToSource
            |> finalizeTx Mainnet costModels localState defaultSelectionAlgo
            |> signTx

For such a tiny contract, which just tests if our signature is present,
no need to put it in a reference UTxO first.
We can embed it directly in the transaction.


## Code Documentation

@docs Tx, Intent, SourceOwner, DestinationOwner, from, to

@docs BasicUtxoSelection, simpleTransfer, transfer

@docs mintAndBurnViaNativeScript, spendFromNativeScript, sendToNativeScript

@docs mintAndBurnViaPlutusScript, ScriptUtxoSelection, spendFromPlutusScript, sendToPlutusScript, withdrawViaPlutusScript

@docs ChangeReallocation, handleChange, changeBackToSource, changeBackTo

@docs addMetadata

@docs constrainTimeValidity

@docs addRequiredSigners

@docs setFeesManually

@docs LocalState, finalizeTx

-}

import Bytes.Comparable exposing (Bytes)
import Cardano.Address exposing (Address, CredentialHash, NetworkId, StakeCredential)
import Cardano.CoinSelection as CoinSelection
import Cardano.Data exposing (Data)
import Cardano.MultiAsset exposing (AssetName, PolicyId)
import Cardano.Redeemer exposing (Redeemer)
import Cardano.Script exposing (NativeScript, PlutusScript)
import Cardano.Transaction exposing (CostModels, Transaction)
import Cardano.Transaction.AuxiliaryData.Metadatum exposing (Metadatum)
import Cardano.Utxo exposing (Output, OutputReference)
import Cardano.Value exposing (Value)
import Integer exposing (Integer)
import Natural exposing (Natural)


{-| -}
type Tx state
    = Tx


{-| -}
type
    Intent
    -- or Action?
    = Intent


{-| -}
type SourceOwner
    = FromCredentialAddress { paymentKey : Bytes CredentialHash, stakeCred : Maybe StakeCredential }
    | FromRewardAddress { stakeKey : Bytes CredentialHash }


{-| -}
type alias DestinationOwner =
    { paymentKey : Bytes CredentialHash, stakeCred : Maybe StakeCredential }


{-| -}
from : Address -> Maybe SourceOwner
from address =
    Debug.todo "from"


{-| -}
to : Address -> Maybe DestinationOwner
to address =
    Debug.todo "to"



-- No script involved


type Needs
    = Needs Never


type HasOrNoNeed
    = HasOrNoNeed Never


initTx : Tx { handleChange : Needs }
initTx =
    Debug.todo "init Tx"


{-| -}
simpleTransfer :
    SourceOwner
    -> DestinationOwner
    -> Value
    -> Tx { handleChange : HasOrNoNeed }
simpleTransfer source destination assets =
    Debug.todo "transfer to someone"


{-| -}
type BasicUtxoSelection
    = AutoUtxoSelection
    | ManualUtxoSelection (List OutputReference)


{-| -}
transfer :
    List { source : SourceOwner, utxoSelection : BasicUtxoSelection, assets : Value }
    -> List { destination : DestinationOwner, assets : Value }
    -> Tx { build | handleChange : Needs }
    -> Tx { build | handleChange : Needs }
transfer sources destinations tx =
    Debug.todo "transfer"



-- Native Script


{-| -}
type NativeScriptSource
    = EmbeddedNativeScript
        { script : NativeScript
        , scriptHash : Bytes CredentialHash
        }
    | ReferencedNativeScript
        { outputRef : OutputReference
        , scriptHash : Bytes CredentialHash
        }


{-| -}
mintAndBurnViaNativeScript :
    NativeScriptSource
    -> List { asset : Bytes AssetName, amount : Integer }
    -> Tx { build | handleChange : Needs }
    -> Tx { build | handleChange : Needs }
mintAndBurnViaNativeScript scriptSource amounts tx =
    Debug.todo "native mint / burn"


{-| -}
sendToNativeScript :
    Bytes CredentialHash
    -> Maybe StakeCredential
    -> Value
    -> Tx { build | handleChange : Needs }
    -> Tx { build | handleChange : Needs }
sendToNativeScript scriptHash maybeStakeCredential assets tx =
    Debug.todo "sendToNativeScript"


{-| -}
spendFromNativeScript :
    NativeScriptSource
    -> BasicUtxoSelection
    -> Value
    -> Tx { build | handleChange : Needs }
    -> Tx { build | handleChange : Needs }
spendFromNativeScript scriptHash utxoSelection assets tx =
    Debug.todo "spendFromNativeScript"



-- Plutus Script


{-| -}
type PlutusScriptSource
    = EmbeddedPlutusScript
        { script : PlutusScript
        , scriptHash : Bytes CredentialHash
        }
    | ReferencedPlutusScript
        { outputRef : OutputReference
        , scriptHash : Bytes CredentialHash
        }


{-| -}
mintAndBurnViaPlutusScript :
    PlutusScriptSource
    -> Data
    -> List { asset : Bytes AssetName, amount : Integer }
    -> Tx { build | handleChange : Needs }
    -> Tx { build | handleChange : Needs }
mintAndBurnViaPlutusScript scriptSource redeemer amounts tx =
    Debug.todo "plutus mint / burn"


{-| -}
sendToPlutusScript :
    Bytes CredentialHash
    -> Maybe StakeCredential
    -> Data
    -> Value
    -> Tx { build | handleChange : Needs }
    -> Tx { build | handleChange : Needs }
sendToPlutusScript scriptHash maybeStakeCredential datum assets tx =
    Debug.todo "send to plutus script"


{-| -}
type ScriptUtxoSelection
    = AutoScriptUtxoSelection ({ ref : OutputReference, utxo : Output } -> Maybe { redeemer : Data })
    | ManualScriptUtxoSelection (List { ref : OutputReference, redeemer : Data })


{-| -}
spendFromPlutusScript : Bytes CredentialHash -> ScriptUtxoSelection -> Value -> Intent
spendFromPlutusScript scriptHash utxoSelection totalSpent =
    Debug.todo "spend from plutus script"


{-| -}
withdrawViaPlutusScript : Bytes CredentialHash -> Data -> Natural -> Intent
withdrawViaPlutusScript scriptHash redeemer adaLovelaces =
    Debug.todo "withdraw via plutus script"



-- Handling change for non-allocated values from spent utxos


{-| -}
type alias ChangeReallocation =
    { toOwners : List ( DestinationOwner, Value )
    , toNativeScripts : List { scripthHash : Bytes CredentialHash, stakeCred : Maybe StakeCredential, assets : Value }
    , toPlutusScripts : List { scripthHash : Bytes CredentialHash, stakeCred : Maybe StakeCredential, datum : Data, assets : Value }
    }


{-| -}
handleChange :
    (List ( Output, Value ) -> ChangeReallocation)
    -> Tx { build | handleChange : Needs }
    -> Tx { build | handleChange : HasOrNoNeed }
handleChange reallocateChange =
    Debug.todo "handle change"


{-| -}
changeBackToSource : List ( Output, Value ) -> ChangeReallocation
changeBackToSource change =
    Debug.todo "change back to source"


{-| -}
changeBackTo : DestinationOwner -> List ( Output, Value ) -> ChangeReallocation
changeBackTo destination change =
    Debug.todo "change back to"



-- Metadata


{-| -}
addMetadata : List ( Natural, Metadatum ) -> Intent
addMetadata metadata =
    Debug.todo "add metadata"



-- Constraints


{-| -}
constrainTimeValidity : { start : Int, end : Natural } -> Intent
constrainTimeValidity { start, end } =
    Debug.todo "time validity"


{-| -}
addRequiredSigners : List (Bytes CredentialHash) -> Intent
addRequiredSigners signers =
    Debug.todo "required signers"



-- Requirements


{-| -}
setFeesManually : Natural -> Intent
setFeesManually adaLovelaces =
    Debug.todo "manual fees"



-- Finalizing the Tx
-- = AutoScriptUtxoSelection ({ ref : OutputReference, utxo : Output } -> Maybe { redeemer : Data })


{-| -}
type alias LocalState =
    { utxos : List ( OutputReference, Output ) }


{-| Finalize a transaction before signing and sending it.

Analyze all intents and perform the following actions:

  - Check the Tx balance
  - Select the input UTxOs
  - Evaluate script execution costs
  - Compute Tx fee

-}
finalizeTx :
    NetworkId
    -> CostModels
    -> LocalState
    -> CoinSelection.Algorithm
    -> Tx { build | handleChange : HasOrNoNeed }
    -> Result String (Tx { build | final : () })
finalizeTx networkId costModels localState selectionAlgo tx =
    Debug.todo "finalize tx"
