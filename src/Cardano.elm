module Cardano exposing
    ( Tx, Intent, SourceOwner(..), DestinationOwner(..), from, to
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

This API revolves around composing intents, then adding metadata and requirements,
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

More control is possible if we want to have multiple senders and receivers.
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

To mint or burn via a native script, here is what we can do.

    dogPolicy =
        hashOfDogNativeScript

    catPolicy =
        hashOfCatNativeScript

    mintDogAndBurnCatTx =
        initTx
            -- minting amounts are of type Integer: unbounded positive or negative integers
            |> mintAndBurnViaNativeScript dogPolicy [ { asset = dogAssetName, amount = Integer.one } ]
            |> mintAndBurnViaNativeScript catPolicy [ { asset = catAssetName, amount = Integer.negate Integer.one } ]
            |> transfer
                [ { source = fromMe
                  , utxoSelection = AutoUtxoSelection
                  , assets = Value.onlyToken catPolicy catAssetName Natural.one
                  }
                ]
                [ { destination = toMe
                  , assets = Value.onlyToken dogPolicy dogAssetName Natural.one
                  }
                ]
            |> finalizeTx Mainnet costModels localState defaultSelectionAlgo
            |> signTx


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
import Cardano.Transaction exposing (CostModels, Transaction)
import Cardano.Transaction.AuxiliaryData.Metadatum exposing (Metadatum)
import Cardano.Utxo exposing (Output, OutputReference)
import Cardano.Value exposing (Value)
import Integer exposing (Integer)
import Natural exposing (Natural)


{-| -}
type Tx
    = Tx


{-| -}
type
    Intent
    -- or Action?
    = Intent


{-| -}
type SourceOwner
    = FromCredentialAddress { paymentKey : Bytes CredentialHash, stakeKey : Maybe (Bytes CredentialHash) }
    | FromRewardAddress { stakeKey : Bytes CredentialHash }


{-| -}
type DestinationOwner
    = ToCredentialAddress { paymentKey : Bytes CredentialHash, stakeKey : Maybe (Bytes CredentialHash) }


{-| -}
from : Address -> Maybe SourceOwner
from address =
    Debug.todo "from"


{-| -}
to : Address -> Maybe DestinationOwner
to address =
    Debug.todo "to"



-- No script involved


{-| -}
type BasicUtxoSelection
    = AutoUtxoSelection
    | ManualUtxoSelection (List OutputReference)


{-| -}
simpleTransfer : SourceOwner -> DestinationOwner -> Value -> Intent
simpleTransfer source destination assets =
    Debug.todo "transfer to someone"


{-| -}
transfer : List { source : SourceOwner, utxoSelection : BasicUtxoSelection, assets : Value } -> List { destination : DestinationOwner, assets : Value } -> Intent
transfer sources destinations =
    Debug.todo "transfer"



-- Native Script


{-| -}
mintAndBurnViaNativeScript : Bytes PolicyId -> List { asset : Bytes AssetName, amount : Integer } -> Intent
mintAndBurnViaNativeScript policy amounts =
    Debug.todo "native mint / burn"


{-| -}
spendFromNativeScript : Bytes CredentialHash -> BasicUtxoSelection -> Value -> Intent
spendFromNativeScript scriptHash utxoSelection assets =
    Debug.todo "spendFromNativeScript"


{-| -}
sendToNativeScript : Bytes CredentialHash -> Maybe StakeCredential -> Value -> Intent
sendToNativeScript scriptHash maybeStakeCredential assets =
    Debug.todo "sendToNativeScript"



-- Plutus Script


{-| -}
mintAndBurnViaPlutusScript : Bytes PolicyId -> Data -> List { asset : Bytes AssetName, amount : Integer } -> Intent
mintAndBurnViaPlutusScript policy redeemerData amounts =
    Debug.todo "plutus mint / burn"


{-| -}
type ScriptUtxoSelection
    = AutoScriptUtxoSelection ({ ref : OutputReference, utxo : Output } -> Maybe { redeemer : Data })
    | ManualScriptUtxoSelection (List { ref : OutputReference, redeemer : Data })


{-| -}
spendFromPlutusScript : Bytes CredentialHash -> ScriptUtxoSelection -> Value -> Intent
spendFromPlutusScript scriptHash utxoSelection totalSpent =
    Debug.todo "spend from plutus script"


{-| -}
sendToPlutusScript : Bytes CredentialHash -> Maybe StakeCredential -> Data -> Value -> Intent
sendToPlutusScript scriptHash maybeStakeCredential datum assets =
    Debug.todo "send to plutus script"


{-| -}
withdrawViaPlutusScript : Bytes CredentialHash -> Natural -> Intent
withdrawViaPlutusScript scriptHash adaLovelaces =
    Debug.todo "withdraw via plutus script"



-- Handling change for non-allocated values from spent utxos


{-| -}
type alias ChangeReallocation =
    { toOwners : List ( DestinationOwner, Value )
    , toNativeScripts : List { scripthHash : Bytes CredentialHash, stakeCredential : Maybe StakeCredential, assets : Value }
    , toPlutusScripts : List { scripthHash : Bytes CredentialHash, stakeCredential : Maybe StakeCredential, datum : Data, assets : Value }
    }


{-| -}
handleChange : (List ( Output, Value ) -> ChangeReallocation) -> Intent
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
finalizeTx : NetworkId -> CostModels -> LocalState -> CoinSelection.Algorithm -> List Intent -> Result String Tx
finalizeTx networkId costModels localState selectionAlgo intents =
    Debug.todo "finalize tx"
