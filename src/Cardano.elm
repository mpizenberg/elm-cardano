module Cardano exposing
    ( TxIntent(..), SpendSource(..), InputsOutputs, ScriptWitness(..), PlutusScriptWitness, WitnessSource(..)
    , TxOtherInfo(..)
    , finalize
    )

{-| Cardano stuff


# Transaction Building Overview

In order to provide elegant transaction building blocks,
we must understand what transactions are.
Here is an example framework composed of 4 points:

1.  Intent: what we want to achieve with this transaction
      - Transfer: send some tokens from somewhere to somewhere else
      - Mint and burn: create and destroy tokens
      - Use a script: provide/spend tokens and data to/from a script
      - Stake management: collect rewards, manage delegations and pool registrations
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

Here is a simple way to send 1 Ada to someone else.

    oneAda =
        -- Asset amounts are typed with unbounded Natural numbers
        Value.onlyLovelace (Natural.fromSafeString "1000000")

    -- Some config required for Tx finalization
    ({ localStateUtxos, costModels, coinSelectionAlgo } as config) =
        Debug.todo "{ localStateUtxos, costModels, coinSelectionAlgo }"

    sendToSomeoneTx =
        [ Spend <| From me oneAda, SendTo you oneAda ]
            |> finalize config []

The finalization step validates the Tx, compute the fees and add other required fields.

More control on the transfer is possible if we want to have multiple senders and receivers.
Here is an example where me and you both contribute 1 Ada.

    twoAda =
        Value.add oneAda oneAda

    bothSendToSomeoneTx =
        [ Spend <| From me oneAda
        , Spend <| From you oneAda
        , SendTo someone twoAda
        ]
            |> finalize config []

To mint or burn via a native script, here is what we can do.

    ( dogOutputRef, dogPolicyId, dogAssetName ) =
        Debug.todo "dog info is provided"

    ( catOutputRef, catPolicyId, catAssetName ) =
        Debug.todo "cat info is provided"

    mintAndBurnTx =
        -- minting 1 dog (amounts are of type Integer: unbounded positive or negative integers)
        [ MintBurn
            { policyId = dogPolicyId
            , assets = Map.singleton dogAssetName Integer.one
            , scriptWitness = NativeWitness (WitnessReference dogOutputRef)
            }
        , SendTo me (Value.onlyToken catPolicyId catAssetName Natural.one)

        -- burning 1 cat
        , Spend <| From me (Value.onlyToken catPolicyId catAssetName Natural.one)
        , MintBurn
            { policyId = catPolicyId
            , assets = Map.singleton catAssetName Integer.negativeOne
            , scriptWitness = NativeWitness (WitnessReference catOutputRef)
            }
        ]
            |> finalize config []

Let’s show how to use a native script to lock some tokens,
that can only be retrieved with our signature.

    -- Retrieve my public key credential from the address
    myKeyCred =
        Address.extractPubKeyHash me
            |> Maybe.withDefault dummyCredential

    -- Native script to lock funds with our public key
    lockScript =
        ScriptPubkey myKeyCred

    lockScriptHash =
        -- `computeNativeScriptHash` will be provided by elm-cardano
        computeNativeScriptHash lockScript

    -- Deriving the script address from the lock script hash
    scriptAddress =
        Address.Shelley
            { networkId = Mainnet
            , paymentCredential = ScriptHash lockScriptHash

            -- Adding our stake credential while we are at it
            -- so that our ada stays staked and yields rewards
            , stakeCredential = Address.extractStakeCredential me
            }

    nativeLockTx =
        [ Spend <| From me twoAda, SendTo scriptAddress twoAda ]
            |> finalize config []

As you can see, we could even keep our stake credential
while locking our ada into the script address,
meaning the locked ada will still be counted in our stake for the rewards.
This is thanks to Cardano addresses which have two parts.
The native script logic only affects the first part of the address.

Let’s show an example how to spend utxos from this native script.
We want to retrieve 1 ada from it, and keep the other ada locked.

    lockedUtxo =
        Debug.todo "the locked utxo with 2 ada"

    nativeUnlockTx =
        -- This native script is so small,
        -- the easiest way to provide it is directly by value
        [ Spend <|
            FromNativeScript
                { spentInput = lockedUtxo
                , nativeScriptWitness = WitnessValue lockScript
                }

        -- Retrieve 1 ada and send 1 ada back to the contract
        , SendTo me oneAda
        , SendTo scriptAddress oneAda
        ]
            |> finalize config []

Alright, how about doing all those things with Plutus scripts now?
Plutus scripts can be used for many purposes such as minting,
spending funds or withdrawing staking rewards.

All script executions need to provide a "redeemer".
This is some mandatory piece of data provided as argument to the script function.
Transaction signatures required by the script must also be specified in a dedicated field.
This enables very efficient script executions since they can just check
that a public key is present in that `requiredSigners` field.

Let’s start with a simple minting and burning example.
For this example, we suppose the plutus script was already written.
This plutus script will accept any mint or burn
as long as we present our signature in the transaction.
The redeemer is not used at all so we can define a dummy one,
of the smallest size possible.

    ( dogScriptOutputRef, dogPolicyId, dogAssetName ) =
        Debug.todo "dog info is provided"

    ( catScriptOutputRef, catPolicyId, catAssetName ) =
        Debug.todo "cat info is provided"

    myKeyCred =
        Address.extractPubKeyHash me
            |> Maybe.withDefault dummyCredential

    -- Dummy redeemer of the smallest size possible.
    -- A redeemer is mandatory, but unchecked by this contract anyway.
    dummyRedeemer =
        Data.Int Integer.zero

    mintAndBurnTx =
        -- minting 1 dog
        [ MintBurn
            { policyId = dogPolicyId
            , assets = Map.singleton dogAssetName Integer.one
            , scriptWitness =
                PlutusWitness
                    { script = WitnessReference dogScriptOutputRef
                    , redeemerData = \_ -> dummyRedeemer
                    , requiredSigners = [ myKeyCred ]
                    }
            }
        , SendTo me (Value.onlyToken dogPolicyId dogAssetName Natural.one)

        -- burning 1 cat
        , Spend <| From me (Value.onlyToken catPolicyId catAssetName Natural.one)
        , MintBurn
            { policyId = catPolicyId
            , assets = Map.singleton catAssetName Integer.negativeOne
            , scriptWitness =
                PlutusWitness
                    { script = WitnessReference catScriptOutputRef
                    , redeemerData = \_ -> dummyRedeemer
                    , requiredSigners = [ myKeyCred ]
                    }
            }
        ]
            |> finalize config []

Ok now let’s show how sending to a Plutus script would be done.
As before, we’ll use the simple example of a lock script.
But this time, we don’t write it directly (as in the NativeScript example).
Instead we suppose the script was written in some onchain language (Aiken, plu-ts, Opshin, ...),
and the blueprint of the script is available, with its hash.

In the eUTxO model, UTxOs created at a script address must have a piece of data attached.
That piece of data is referred to as the "datum".
It will be passed as argument to the script execution, in addition to the redeemer.

    lockScriptHash =
        extractedFromBlueprint

    scriptAddress =
        Address.Shelley
            { networkId = Mainnet
            , paymentCredential = ScriptHash lockScriptHash

            -- This is our staking credential
            -- We use it to keep our locked ada staked!
            , stakeCredential = Address.extractStakeCredential me
            }

    myKeyCred =
        Address.extractPubKeyHash me
            |> Maybe.withDefault dummyCredential

    -- Put the unlocking pubkey in the datum of the funds we lock
    datumWithKeyCred =
        Data.Bytes (Bytes.toAny myKeyCred)

    lockInPlutusScriptTx =
        [ Spend <| From me twoAda
        , SendToOutput
            (\_ ->
                { address = scriptAddress
                , amount = twoAda
                , datumOption = Just (Datum datumWithKeyCred)
                , referenceScript = Nothing
                }
            )
        ]
            |> finalize config []

You may have noticed that `SendToOutput` is taking a function parameter
instead of just an `Output`.
This is to enable more advanced use cases such as [UTxO indexers][utxo-indexers].
But for simple use cases, we can just ignore that argument with an underscore `_`.

[utxo-indexers]: https://github.com/Anastasia-Labs/aiken-design-patterns

Now that we know how to send values to a script, let’s see how to collect them.
We will show how to retrieve 1 ada from the previously locked 2 ada.
For that, we need to do a few things:

1.  Spend the whole UTxO, with its 2 ada in it.
    We cannot partially spend UTxOs.
2.  Provide the script code to the transaction.
    The script hash must match with the first part of the UTxO address we are spending.
3.  Provide our signature for the proof that the script needs.
4.  Retrieve 1 ada from that spent UTxO, and send 1 ada back to the same script.

For such a tiny script, which just checks if our signature is present,
no need to put it in a reference UTxO first.
We can embed it directly in the transaction witness.

    ( lockScript, lockScriptHash ) =
        Debug.todo "Extracted from the script blueprint"

    unlockFromPlutusScriptTx =
        -- Collect 1 ada from the locked UTxO at the script address
        [ Spend <|
            FromPlutusScript
                { spentInput = Debug.todo "the locked utxo with 2 ada"
                , datumWitness = Nothing -- not needed, the datum was given by value
                , plutusScriptWitness =
                    { script = WitnessValue lockScript -- script passed by value
                    , redeemerData = \_ -> dummyRedeemer -- unused
                    , requiredSigners = [ myCredential ]
                    }
                }
        , SendTo me oneAda

        -- Return the other 1 ada to the lock script (there was 2 ada initially)
        , SendToOutput
            (\_ ->
                { address = scriptAddress
                , amount = oneAda
                , datumOption = Just (Datum datumWithKeyCred)
                , referenceScript = Nothing
                }
            )
        ]
            |> finalize config []


## Code Documentation

@docs TxIntent, SpendSource, InputsOutputs, ScriptWitness, PlutusScriptWitness, WitnessSource
@docs TxOtherInfo
@docs finalize

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as Map exposing (BytesMap)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..), StakeAddress, StakeCredential(..))
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset exposing (AssetName, PolicyId)
import Cardano.Redeemer exposing (Redeemer)
import Cardano.Script exposing (NativeScript, PlutusScript)
import Cardano.Transaction exposing (CostModels, Transaction)
import Cardano.Transaction.AuxiliaryData.Metadatum exposing (Metadatum)
import Cardano.Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Integer exposing (Integer)
import Natural exposing (Natural)


type Todo
    = Todo


{-| -}
type TxIntent
    = SendTo Address Value
    | SendToOutput (InputsOutputs -> Output)
      -- Spending assets from somewhere
    | Spend SpendSource
      -- Minting / burning assets
    | MintBurn
        { policyId : Bytes CredentialHash
        , assets : BytesMap AssetName Integer
        , scriptWitness : ScriptWitness
        }
      -- Issuing certificates
    | IssueCertificate Todo
      -- Withdrawing rewards
    | WithdrawRewards
        { stakeCredential : StakeAddress
        , amount : Natural
        , scriptWitness : Maybe ScriptWitness
        }


{-| -}
type SpendSource
    = From Address Value
    | FromWalletUtxo OutputReference
    | FromNativeScript
        { spentInput : OutputReference
        , nativeScriptWitness : WitnessSource NativeScript
        }
    | FromPlutusScript
        { spentInput : OutputReference
        , datumWitness : Maybe (WitnessSource Data)
        , plutusScriptWitness : PlutusScriptWitness
        }


{-| -}
type alias InputsOutputs =
    { referenceInputs : List OutputReference
    , spentInputs : List OutputReference
    , createdOutputs : List Output
    }


{-| -}
type ScriptWitness
    = NativeWitness (WitnessSource NativeScript)
    | PlutusWitness PlutusScriptWitness


{-| -}
type alias PlutusScriptWitness =
    { script : WitnessSource PlutusScript
    , redeemerData : InputsOutputs -> Data
    , requiredSigners : List (Bytes CredentialHash)
    }


{-| -}
type WitnessSource a
    = WitnessValue a
    | WitnessReference OutputReference


{-| -}
type TxOtherInfo
    = TxReferenceInput OutputReference
    | TxMetadata { tag : Natural, metadata : Metadatum }
    | TxTimeValidityRange { start : Int, end : Natural }
    | TxManualFee { lovelace : Natural }


{-| Finalize a transaction before signing and sending it.

Analyze all intents and perform the following actions:

  - Check the Tx balance
  - Select the input UTxOs
  - Evaluate script execution costs
  - Compute Tx fee

-}
finalize :
    { localStateUtxos : List ( OutputReference, Output )
    , costModels : CostModels
    , coinSelectionAlgo : CoinSelection.Algorithm
    }
    -> List TxOtherInfo
    -> List TxIntent
    -> Result String Transaction
finalize { localStateUtxos, costModels, coinSelectionAlgo } txOtherInfo txIntents =
    Debug.todo "finalize"



-- EXAMPLES ##########################################################


makeWalletAddress name =
    Address.Shelley
        { networkId = Mainnet
        , paymentCredential = VKeyHash (Bytes.fromStringUnchecked name)
        , stakeCredential = Just (InlineCredential (VKeyHash <| Bytes.fromStringUnchecked name))
        }


oneAda =
    -- Asset amounts are typed with unbounded Natural numbers
    Value.onlyLovelace (Natural.fromSafeString "1000000")



-- EXAMPLE 1: Simple transfer


example1 _ =
    let
        ({ localStateUtxos, costModels, coinSelectionAlgo } as config) =
            Debug.todo "{ localStateUtxos, costModels, coinSelectionAlgo }"
    in
    [ Spend <| From (makeWalletAddress "me") oneAda
    , SendTo (makeWalletAddress "you") oneAda
    ]
        |> finalize config []



-- EXAMPLE 2: mint/burn with native script


example2 _ =
    let
        me =
            makeWalletAddress "me"

        ( dogOutputRef, dogPolicyId, dogAssetName ) =
            Debug.todo "dog info is provided"

        ( catOutputRef, catPolicyId, catAssetName ) =
            Debug.todo "cat info is provided"

        ({ localStateUtxos, costModels, coinSelectionAlgo } as config) =
            Debug.todo "{ localStateUtxos, costModels, coinSelectionAlgo }"
    in
    -- minting 1 dog (amounts are of type Integer: unbounded positive or negative integers)
    [ MintBurn
        { policyId = dogPolicyId
        , assets = Map.singleton dogAssetName Integer.one
        , scriptWitness = NativeWitness (WitnessReference dogOutputRef)
        }
    , SendTo me (Value.onlyToken catPolicyId catAssetName Natural.one)

    -- burning 1 cat
    , Spend <| From me (Value.onlyToken catPolicyId catAssetName Natural.one)
    , MintBurn
        { policyId = catPolicyId
        , assets = Map.singleton catAssetName Integer.negativeOne
        , scriptWitness = NativeWitness (WitnessReference catOutputRef)
        }
    ]
        |> finalize config []



-- EXAMPLE 3: spend from a Plutus script


makeScriptAddress scriptHash maybeStakeCredential =
    Address.Shelley
        { networkId = Mainnet
        , paymentCredential = ScriptHash scriptHash
        , stakeCredential = maybeStakeCredential
        }


example3 _ =
    let
        ( me, myCredential, myStakeCred ) =
            ( makeWalletAddress "me"
            , makeWalletAddress "me"
                |> Address.extractPubKeyHash
                |> Maybe.withDefault (Debug.todo "should not fail")
            , makeWalletAddress "me"
                |> Address.extractStakeCredential
            )

        ( lockScript, lockScriptHash ) =
            Debug.todo "coming from the blueprint"

        -- Dummy redeemer of the smallest size possible.
        -- A redeemer is mandatory, but unchecked by this contract anyway.
        dummyRedeemer =
            Data.Int Integer.zero

        ({ localStateUtxos, costModels, coinSelectionAlgo } as config) =
            Debug.todo "{ localStateUtxos, costModels, coinSelectionAlgo }"
    in
    -- Collect 1 ada from the lock script
    [ Spend <|
        FromPlutusScript
            { spentInput = Debug.todo "the locked utxo with 2 ada"
            , datumWitness = Nothing
            , plutusScriptWitness =
                { script = WitnessValue lockScript
                , redeemerData = \_ -> dummyRedeemer
                , requiredSigners = [ myCredential ]
                }
            }
    , SendTo me oneAda

    -- Return the other 1 ada to the lock script (there was 2 ada initially)
    , SendToOutput
        (\_ ->
            -- Use our own stake credential to keep staking and earning rewards
            { address = makeScriptAddress lockScriptHash myStakeCred
            , amount = oneAda

            -- Add our pubkey credential to the datum of the newly locked utxo
            , datumOption = Just (Datum (Data.Bytes <| Bytes.toAny myCredential))
            , referenceScript = Nothing
            }
        )
    ]
        |> finalize config []
