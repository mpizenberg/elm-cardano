module Cardano exposing
    ( Tx, SourceOwner(..), DestinationOwner, from, to
    , BasicUtxoSelection(..), simpleTransfer, transfer, createOutput
    , mintAndBurnViaNativeScript, sendToNativeScript, spendFromNativeScript
    , mintAndBurnViaPlutusScript, ScriptUtxoSelection(..), sendToPlutusScript, spendFromPlutusScript, addRequiredSigners, withdrawViaPlutusScript
    , ChangeReallocation, handleChange, changeBackToSource, changeBackTo
    , setMetadata, setTimeValidity, payFeesWithAccount
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
But this time, we don’t write it directly (as in the NativeScript example).
Instead we suppose the contract was written in another language (Aiken, plu-ts, Opshin, ...).
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
We can embed it directly in the transaction if we want to spend its UTxOs.

    lockScriptSource =
        -- script and scriptHash are coming from the blueprint
        EmbeddedPlutusScript { script = lockScript, scriptHash = lockScriptHash }

    localState =
        -- We already updated a list of our UTxOs
        { utxos = myUtxos ++ scriptUtxos }

    -- We manually identify the locked UTxO we want to consume
    -- in order to retrieve 1 ada from it.
    lockedUtxoSelection =
        ManualScriptUtxoSelection
            [ { ref = theKnownOutputRef
              , utxo = theKnownUtxo
              , redeemer = Data.Int Integer.zero -- unused here anyway
              }
            ]

    unlockTx =
        initTx
            |> spendFromPlutusScript lockScriptSource lockedUtxoSelection oneAda
            -- required signature for the plutus script to check
            |> addRequiredSigners [ toMe.paymentKey ]
            |> transfer [] [ { destination = toMe, assets = oneAda } ]
            -- need a manual handling of the change this time
            |> handleChange manualChangeReallocation
            |> payFeesWithAccount fromMe AutoComputeFees
            |> finalizeTx Mainnet costModels localState defaultSelectionAlgo
            |> signTx

    -- Send back 1 ada to the lock script.
    -- Retrieve the rest back to me.
    manualChangeReallocation allChange =
        let
            accumulatedChange =
                Value.sum (List.map Tuple.second allChange)

            backToScript =
                oneAda

            restOfChange =
                Value.substract accumulatedChange backToScript
        in
        { toOwners = [ ( toMe, restOfChange ) ]
        , toNativeScripts = []
        , toPlutusScripts =
            [ { scriptHash = lockScriptHash
              , stakeCred = myStakeCredential
              , datum = Data.Bytes (Bytes.toAny toMe.paymentKey)
              , assets = backToScript
              }
            ]
        }

Ok this was a bit more code than before, but I hope it is still very clear.
There are three novelties in the above example.

First, we manually selected the utxo we want to consume to unlock our 1 ada.
Since UTxOs in Plutus scripts have datum attached, they are not super fungible.
We could use an autoselection, with care,
but it’s just simpler to manually tell which one to consume.

Second is the manual change reallocation.
Since we only retrieve 1 ada of the 2 adas, we want to put 1 ada back.
But we don’t want to use the automatic `changeBackToSource` function
that we used up to now.
If coded well, it could default to copy the previous datum,
but if we are just unsure of its behavior, its simpler to manually control the change.

Third, this time we decided to specify who is paying the fees.
It is us here, but it doesn’t have to be.
This way, the finalization step will know where to look to pay for the fees.
The default behavior tries to just use lovelaces from some of the spent UTxOs.


## Code Documentation

@docs Tx, WIP, AlmostReady, SourceOwner, DestinationOwner, from, to

@docs BasicUtxoSelection, simpleTransfer, transfer, createOutput

@docs mintAndBurnViaNativeScript, sendToNativeScript, spendFromNativeScript

@docs mintAndBurnViaPlutusScript, ScriptUtxoSelection, sendToPlutusScript, spendFromPlutusScript, addRequiredSigners, withdrawViaPlutusScript

@docs ChangeReallocation, handleChange, changeBackToSource, changeBackTo

@docs setMetadata, setTimeValidity, payFeesWithAccount

@docs LocalState, finalizeTx

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (Address, CredentialHash, NetworkId(..), StakeCredential)
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset exposing (AssetName, PolicyId)
import Cardano.Redeemer exposing (Redeemer)
import Cardano.Script exposing (NativeScript, PlutusScript)
import Cardano.Transaction exposing (CostModels, Transaction)
import Cardano.Transaction.AuxiliaryData.Metadatum exposing (Metadatum)
import Cardano.Utxo exposing (Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Integer exposing (Integer)
import Natural exposing (Natural)


{-| -}
type Tx state
    = Tx


{-| -}
type WIP
    = WIP Never


{-| -}
type AlmostReady
    = AlmostReady Never


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


initTx : Tx WIP
initTx =
    Debug.todo "init Tx"


{-| -}
simpleTransfer :
    SourceOwner
    -> DestinationOwner
    -> Value
    -> Tx AlmostReady
simpleTransfer source destination assets =
    Debug.todo "transfer to someone"


{-| -}
type BasicUtxoSelection
    = AutoUtxoSelection
      -- TODO: think if the Output is needed or not in manual selection.
      -- In theory not, as long as its output reference
      -- is present in the LocalState we use in finalizeTx.
    | ManualUtxoSelection (List ( OutputReference, Output ))


{-| -}
transfer :
    List { source : SourceOwner, utxoSelection : BasicUtxoSelection, assets : Value }
    -> List { destination : DestinationOwner, assets : Value }
    -> Tx WIP
    -> Tx WIP
transfer sources destinations tx =
    Debug.todo "transfer"


{-| Custom construct for specific needs involving a datum or reference script.
-}
createOutput : Output -> Tx WIP -> Tx WIP
createOutput output tx =
    Debug.todo "create output"



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
    -> Tx WIP
    -> Tx WIP
mintAndBurnViaNativeScript scriptSource amounts tx =
    Debug.todo "native mint / burn"


{-| -}
sendToNativeScript :
    Bytes CredentialHash
    -> Maybe StakeCredential
    -> Value
    -> Tx WIP
    -> Tx WIP
sendToNativeScript scriptHash maybeStakeCredential assets tx =
    Debug.todo "sendToNativeScript"


{-| -}
spendFromNativeScript :
    NativeScriptSource
    -> BasicUtxoSelection
    -> Value
    -> Tx WIP
    -> Tx WIP
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
    -> Tx WIP
    -> Tx WIP
mintAndBurnViaPlutusScript scriptSource redeemer amounts tx =
    Debug.todo "plutus mint / burn"


{-| -}
sendToPlutusScript :
    Bytes CredentialHash
    -> Maybe StakeCredential
    -> Data
    -> Value
    -> Tx WIP
    -> Tx WIP
sendToPlutusScript scriptHash maybeStakeCredential datum assets tx =
    Debug.todo "send to plutus script"


{-| -}
type ScriptUtxoSelection
    = AutoScriptUtxoSelection ({ ref : OutputReference, utxo : Output } -> Maybe { redeemer : Data })
    | ManualScriptUtxoSelection
        (List
            { ref : OutputReference

            -- TODO: think if the Output is needed or not in manual selection.
            -- In theory not, as long as its output reference
            -- is present in the LocalState we use in finalizeTx.
            , utxo : Output
            , redeemer : Data
            }
        )


{-| -}
spendFromPlutusScript :
    PlutusScriptSource
    -> ScriptUtxoSelection
    -> Value
    -> Tx WIP
    -> Tx WIP
spendFromPlutusScript scriptHash utxoSelection totalSpent =
    Debug.todo "spend from plutus script"


{-| -}
addRequiredSigners : List (Bytes CredentialHash) -> Tx WIP -> Tx WIP
addRequiredSigners signers tx =
    Debug.todo "required signers"


{-| -}
withdrawViaPlutusScript :
    Bytes CredentialHash
    -> Data
    -> Natural
    -> Tx WIP
    -> Tx WIP
withdrawViaPlutusScript scriptHash redeemer adaLovelaces tx =
    Debug.todo "withdraw via plutus script"



-- Handling change for non-allocated values from spent utxos


{-| -}
type alias ChangeReallocation =
    { toOwners : List ( DestinationOwner, Value )
    , toNativeScripts :
        List
            { scriptHash : Bytes CredentialHash
            , stakeCred : Maybe StakeCredential
            , assets : Value
            }
    , toPlutusScripts :
        List
            { scriptHash : Bytes CredentialHash
            , stakeCred : Maybe StakeCredential
            , datum : Data
            , assets : Value
            }
    }


{-| -}
handleChange :
    (List ( Output, Value ) -> ChangeReallocation)
    -> Tx WIP
    -> Tx AlmostReady
handleChange reallocateChange tx =
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
setMetadata : List ( Natural, Metadatum ) -> Tx AlmostReady -> Tx AlmostReady
setMetadata metadata tx =
    Debug.todo "add metadata"



-- Constraints


{-| -}
setTimeValidity : { start : Int, end : Natural } -> Tx AlmostReady -> Tx AlmostReady
setTimeValidity { start, end } tx =
    Debug.todo "time validity"



-- Fees


{-| -}
type FeeStrat
    = AutoComputeFees
    | ManualFeeAmount Natural


{-| -}
payFeesWithAccount : SourceOwner -> FeeStrat -> Tx AlmostReady -> Tx AlmostReady
payFeesWithAccount source strat tx =
    Debug.todo "manual fees"



-- Finalizing the Tx


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
    -> Tx AlmostReady
    -> Result String Transaction
finalizeTx networkId costModels localState selectionAlgo tx =
    Debug.todo "finalize tx"



-- EXAMPLES ##########################################################


addressTriplet : a -> ( Address, SourceOwner, DestinationOwner )
addressTriplet _ =
    let
        me : Address
        me =
            Debug.todo "me address"

        fromMe =
            from me
                |> Maybe.withDefault (Debug.todo "shouldNotErrorIfIsAnActualVKeyCredential")

        toMe =
            to me
                |> Maybe.withDefault (Debug.todo "shouldNotErrorIfIsAnActualVKeyCredential")
    in
    ( me, fromMe, toMe )


oneAda =
    -- Asset amounts are typed with unbounded Natural numbers
    Value.onlyLovelace (Natural.fromSafeString "1000000")


twoAda =
    -- Asset amounts are typed with unbounded Natural numbers
    Value.onlyLovelace (Natural.fromSafeString "2000000")



-- EXAMPLE 1: Simple transfer


example1 _ =
    let
        ( me, fromMe, _ ) =
            addressTriplet ()

        ( someone, _, toSomeone ) =
            addressTriplet ()

        ( costModels, localState, defaultSelectionAlgo ) =
            Debug.todo "finalize config"
    in
    simpleTransfer fromMe toSomeone oneAda
        |> finalizeTx Mainnet costModels localState defaultSelectionAlgo



-- EXAMPLE 2: mint/burn with native script


example2 _ =
    let
        ( dogOutputRef, dogPolicyId, dogAssetName ) =
            Debug.todo "dog info is provided"

        ( catOutputRef, catPolicyId, catAssetName ) =
            Debug.todo "cat info is provided"

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

        ( _, fromMe, toMe ) =
            addressTriplet ()

        autoSelectFromMe assets =
            { source = fromMe, utxoSelection = AutoUtxoSelection, assets = assets }

        backToMe assets =
            { destination = toMe, assets = assets }

        ( costModels, localState, defaultSelectionAlgo ) =
            Debug.todo "finalize config"
    in
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



-- EXAMPLE 3: spend from a Plutus script


example3 _ =
    let
        ( me, fromMe, toMe ) =
            addressTriplet ()

        ( lockScript, lockScriptHash ) =
            Debug.todo "coming from the blueprint"

        lockScriptSource =
            EmbeddedPlutusScript { script = lockScript, scriptHash = lockScriptHash }

        -- We manually identify the locked UTxO we want to consume
        -- in order to retrieve 1 ada from it.
        lockedUtxoSelection =
            ManualScriptUtxoSelection
                [ { ref = Debug.todo "theKnownOutputRef"
                  , utxo = Debug.todo "theKnownUtxo"
                  , redeemer = Data.Int Integer.zero -- unused here anyway
                  }
                ]

        -- Send back 1 ada to the lock script.
        -- Retrieve the rest back to me.
        manualChangeReallocation allChange =
            let
                accumulatedChange =
                    Value.sum (List.map Tuple.second allChange)

                backToScript =
                    oneAda

                restOfChange =
                    Value.substract accumulatedChange backToScript
            in
            { toOwners = [ ( toMe, restOfChange ) ]
            , toNativeScripts = []
            , toPlutusScripts =
                [ { scriptHash = lockScriptHash
                  , stakeCred = toMe.stakeCred
                  , datum = Data.Bytes (Bytes.toAny toMe.paymentKey)
                  , assets = backToScript
                  }
                ]
            }

        ( costModels, localState, defaultSelectionAlgo ) =
            Debug.todo "finalize config"
    in
    initTx
        |> spendFromPlutusScript lockScriptSource lockedUtxoSelection oneAda
        -- required signature for the plutus script to check
        |> addRequiredSigners [ toMe.paymentKey ]
        |> transfer [] [ { destination = toMe, assets = oneAda } ]
        -- need a manual handling of the change this time
        |> handleChange manualChangeReallocation
        |> payFeesWithAccount fromMe AutoComputeFees
        |> finalizeTx Mainnet costModels localState defaultSelectionAlgo
