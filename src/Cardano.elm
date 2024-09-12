module Cardano exposing
    ( TxIntent(..), SpendSource(..), InputsOutputs, ScriptWitness(..), PlutusScriptWitness, WitnessSource(..)
    , TxOtherInfo(..)
    , Fee(..)
    , finalize, TxFinalizationError(..)
    , example1, example2, example3, prettyTx
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
    ({ localStateUtxos, coinSelectionAlgo } as config) =
        Debug.todo "{ localStateUtxos, coinSelectionAlgo }"

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
                    , requiredSigners = [ myKeyCred ]
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
@docs Fee
@docs finalize, TxFinalizationError

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as Map exposing (BytesMap)
import Cardano.Address as Address exposing (Address(..), Credential(..), CredentialHash, NetworkId(..), StakeAddress, StakeCredential(..))
import Cardano.Cip30 exposing (Utxo)
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset as MultiAsset exposing (AssetName, MultiAsset, PolicyId)
import Cardano.Redeemer as Redeemer exposing (Redeemer, RedeemerTag)
import Cardano.Script as Script exposing (NativeScript, PlutusScript, PlutusVersion(..), ScriptCbor)
import Cardano.Transaction as Transaction exposing (ScriptDataHash, Transaction, TransactionBody, VKeyWitness, WitnessSet)
import Cardano.Transaction.AuxiliaryData exposing (AuxiliaryData)
import Cardano.Transaction.AuxiliaryData.Metadatum as Metadatum exposing (Metadatum)
import Cardano.Transaction.Builder exposing (requiredSigner, totalCollateral)
import Cardano.Uplc as Uplc exposing (VmConfig, evalScriptsCosts)
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Cbor.Encode as E
import Dict exposing (Dict)
import Dict.Any exposing (AnyDict)
import Integer exposing (Integer)
import Natural exposing (Natural)
import Set


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
      -- (Maybe) Eventually improve "From Address Value"" with variants like:
      -- FromAnywhere Value
      -- FromPaymentKey (Bytes CredentialHash)
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


{-| Helper initialization for InputsOutputs.
-}
noInputsOutputs : InputsOutputs
noInputsOutputs =
    { referenceInputs = [], spentInputs = [], createdOutputs = [] }


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


{-| Extract the [OutputReference] from a witness source,
if passed by reference. Return [Nothing] if passed by value.
-}
extractWitnessRef : WitnessSource a -> Maybe OutputReference
extractWitnessRef witnessSource =
    case witnessSource of
        WitnessValue _ ->
            Nothing

        WitnessReference ref ->
            Just ref


{-| -}
type TxOtherInfo
    = TxReferenceInput OutputReference
    | TxMetadata { tag : Natural, metadata : Metadatum }
    | TxTimeValidityRange { start : Int, end : Natural }


{-| -}
type Fee
    = ManualFee (List { paymentSource : Address, exactFeeAmount : Natural })
    | AutoFee { paymentSource : Address }


{-| Initialize fee estimation by setting the fee field to ₳0.5
This is represented as 500K lovelace, which is encoded as a 32bit uint.
32bit uint can represent a range from ₳0.065 to ₳4200 so it most likely won’t change.
-}
defaultAutoFee : Natural
defaultAutoFee =
    Natural.fromSafeInt 500000


{-| Errors that may happen during Tx finalization.
-}
type TxFinalizationError
    = UnbalancedIntents String
    | InsufficientManualFee { declared : Natural, computed : Natural }
    | NotEnoughMinAda String
    | ReferenceOutputsMissingFromLocalState (List OutputReference)
    | FailedToPerformCoinSelection CoinSelection.Error
    | CollateralSelectionError CoinSelection.Error
    | DuplicatedMetadataTags Int
    | IncorrectTimeValidityRange String
    | UplcVmError String
    | FailurePleaseReportToElmCardano String


{-| Finalize a transaction before signing and sending it.

Analyze all intents and perform the following actions:

  - Check the Tx balance
  - Select the input UTxOs
  - Evaluate script execution costs
  - Compute Tx fee

-}
finalize :
    { localStateUtxos : Utxo.RefDict Output
    , coinSelectionAlgo : CoinSelection.Algorithm
    , evalScriptsCosts : Utxo.RefDict Output -> Transaction -> Result String (List Redeemer)
    }
    -> Fee
    -> List TxOtherInfo
    -> List TxIntent
    -> Result TxFinalizationError Transaction
finalize { localStateUtxos, coinSelectionAlgo, evalScriptsCosts } fee txOtherInfo txIntents =
    case ( processIntents localStateUtxos txIntents, processOtherInfo txOtherInfo ) of
        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err

        ( Ok processedIntents, Ok processedOtherInfo ) ->
            let
                buildTxRound : InputsOutputs -> Fee -> Result TxFinalizationError Transaction
                buildTxRound roundInputsOutputs roundFees =
                    let
                        ( feeAmount, feeAddresses ) =
                            case roundFees of
                                ManualFee perAddressFee ->
                                    ( List.foldl (\{ exactFeeAmount } -> Natural.add exactFeeAmount) Natural.zero perAddressFee
                                    , List.map .paymentSource perAddressFee
                                    )

                                AutoFee { paymentSource } ->
                                    ( defaultAutoFee, [ paymentSource ] )

                        ( collateralAmount, collateralSources ) =
                            if List.isEmpty processedIntents.plutusScriptSources then
                                ( Natural.zero, Address.emptyDict )

                            else
                                -- collateral = 1.5 * fee
                                -- It’s an euclidean division, so if there is a non-zero rest,
                                -- we add 1 to make sure we aren’t short 1 lovelace.
                                ( feeAmount
                                    |> Natural.mul (Natural.fromSafeInt 15)
                                    |> Natural.divModBy (Natural.fromSafeInt 10)
                                    |> Maybe.withDefault ( Natural.zero, Natural.zero )
                                    |> (\( q, r ) -> Natural.add q <| Natural.min r Natural.one)
                                  -- Identify automatically collateral sources
                                  -- from fee addresses, free inputs addresses or spent inputs addresses.
                                , [ feeAddresses
                                  , Dict.Any.keys processedIntents.freeInputs
                                  , Dict.Any.keys processedIntents.preSelected.inputs
                                        |> List.filterMap (\addr -> Dict.Any.get addr localStateUtxos |> Maybe.map .address)
                                  ]
                                    |> List.concat
                                    |> List.filter Address.isShelleyWallet
                                    -- make the list unique
                                    |> List.map (\addr -> ( addr, () ))
                                    |> Address.dictFromList
                                )
                    in
                    -- UTxO selection
                    Result.map2
                        (\coinSelection collateralSelection ->
                            --> coinSelection : Address.Dict (Selection, List Output)
                            -- Accumulate all selected UTxOs and newly created outputs
                            accumPerAddressSelection coinSelection
                                --> { selectedInputs : Utxo.RefDict Ouptut, createdOutputs : List Output }
                                -- Aggregate with pre-selected inputs and pre-created outputs
                                |> (\selection -> updateInputsOutputs processedIntents selection roundInputsOutputs)
                                --> InputsOutputs
                                |> buildTx localStateUtxos feeAmount collateralSelection processedIntents processedOtherInfo
                        )
                        (computeCoinSelection localStateUtxos roundFees processedIntents coinSelectionAlgo)
                        (computeCollateralSelection localStateUtxos collateralSources collateralAmount)

                extractInputsOutputs tx =
                    { referenceInputs = tx.body.referenceInputs
                    , spentInputs = tx.body.inputs
                    , createdOutputs = tx.body.outputs
                    }

                adjustFees tx =
                    case fee of
                        ManualFee _ ->
                            fee

                        AutoFee { paymentSource } ->
                            Transaction.computeFees tx
                                |> Debug.log "estimatedFee"
                                |> (\computedFee -> ManualFee [ { paymentSource = paymentSource, exactFeeAmount = computedFee } ])
            in
            -- Without estimating cost of plutus script exec, do few loops of:
            --   - estimate Tx fees
            --   - adjust coin selection
            --   - adjust redeemers
            buildTxRound noInputsOutputs fee
                --> Result String Transaction
                |> Result.andThen (\tx -> buildTxRound (extractInputsOutputs tx) (adjustFees tx))
                -- TODO: Evaluate plutus script cost, and do a final round of above
                |> Result.andThen (adjustExecutionCosts <| evalScriptsCosts localStateUtxos)
                -- Finally, check if final fees are correct
                |> Result.andThen (checkInsufficientFee fee)
                |> identity


type alias PreProcessedIntents =
    { freeInputs : Address.Dict Value
    , freeOutputs : Address.Dict Value
    , preSelected : List { input : OutputReference, redeemer : Maybe (InputsOutputs -> Data) }
    , preCreated : InputsOutputs -> { sum : Value, outputs : List Output }
    , nativeScriptSources : List (WitnessSource NativeScript)
    , plutusScriptSources : List (WitnessSource PlutusScript)
    , datumSources : List (WitnessSource Data)
    , requiredSigners : List (List (Bytes CredentialHash))
    , mints : List { policyId : Bytes CredentialHash, assets : BytesMap AssetName Integer, redeemer : Maybe (InputsOutputs -> Data) }
    , withdrawals : List { stakeAddress : StakeAddress, amount : Natural, redeemer : Maybe (InputsOutputs -> Data) }
    }


noIntent : PreProcessedIntents
noIntent =
    { freeInputs = Address.emptyDict
    , freeOutputs = Address.emptyDict
    , preSelected = []
    , preCreated = \_ -> { sum = Value.zero, outputs = [] }
    , nativeScriptSources = []
    , plutusScriptSources = []
    , datumSources = []
    , requiredSigners = []
    , mints = []
    , withdrawals = []
    }


{-| Initial processing step in order to categorize all intents.

This pre-processing step does not need the local utxo state.
It only aggregates all intents into relevant fields
to make following processing steps easier.

-}
preProcessIntents : List TxIntent -> PreProcessedIntents
preProcessIntents txIntents =
    let
        freeValueAdd : Address -> Value -> Address.Dict Value -> Address.Dict Value
        freeValueAdd addr v freeValue =
            Dict.Any.update addr (Just << Value.add v << Maybe.withDefault Value.zero) freeValue

        -- Step function that pre-processes each TxIntent
        stepIntent : TxIntent -> PreProcessedIntents -> PreProcessedIntents
        stepIntent txIntent preProcessedIntents =
            case txIntent of
                SendTo addr v ->
                    { preProcessedIntents
                        | freeOutputs = freeValueAdd addr v preProcessedIntents.freeOutputs
                    }

                SendToOutput f ->
                    let
                        newPreCreated inputsOutputs =
                            let
                                { sum, outputs } =
                                    preProcessedIntents.preCreated inputsOutputs

                                newOutput =
                                    f inputsOutputs
                            in
                            { sum = Value.add sum newOutput.amount
                            , outputs = newOutput :: outputs
                            }
                    in
                    { preProcessedIntents | preCreated = newPreCreated }

                Spend (From addr v) ->
                    { preProcessedIntents
                        | freeInputs = freeValueAdd addr v preProcessedIntents.freeInputs
                    }

                Spend (FromWalletUtxo ref) ->
                    { preProcessedIntents | preSelected = { input = ref, redeemer = Nothing } :: preProcessedIntents.preSelected }

                Spend (FromNativeScript { spentInput, nativeScriptWitness }) ->
                    { preProcessedIntents
                        | preSelected = { input = spentInput, redeemer = Nothing } :: preProcessedIntents.preSelected
                        , nativeScriptSources = nativeScriptWitness :: preProcessedIntents.nativeScriptSources
                    }

                Spend (FromPlutusScript { spentInput, datumWitness, plutusScriptWitness }) ->
                    let
                        newDatumSources =
                            case datumWitness of
                                Nothing ->
                                    preProcessedIntents.datumSources

                                Just datumSource ->
                                    datumSource :: preProcessedIntents.datumSources
                    in
                    { preProcessedIntents
                        | preSelected = { input = spentInput, redeemer = Just plutusScriptWitness.redeemerData } :: preProcessedIntents.preSelected
                        , datumSources = newDatumSources
                        , requiredSigners = plutusScriptWitness.requiredSigners :: preProcessedIntents.requiredSigners
                        , plutusScriptSources = plutusScriptWitness.script :: preProcessedIntents.plutusScriptSources
                    }

                MintBurn { policyId, assets, scriptWitness } ->
                    case scriptWitness of
                        NativeWitness script ->
                            { preProcessedIntents
                                | nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                                , mints = { policyId = policyId, assets = assets, redeemer = Nothing } :: preProcessedIntents.mints
                            }

                        PlutusWitness { script, redeemerData, requiredSigners } ->
                            { preProcessedIntents
                                | plutusScriptSources = script :: preProcessedIntents.plutusScriptSources
                                , requiredSigners = requiredSigners :: preProcessedIntents.requiredSigners
                                , mints = { policyId = policyId, assets = assets, redeemer = Just redeemerData } :: preProcessedIntents.mints
                            }

                WithdrawRewards { stakeCredential, amount, scriptWitness } ->
                    case scriptWitness of
                        Nothing ->
                            { preProcessedIntents
                                | withdrawals = { stakeAddress = stakeCredential, amount = amount, redeemer = Nothing } :: preProcessedIntents.withdrawals
                            }

                        Just (NativeWitness script) ->
                            { preProcessedIntents
                                | withdrawals = { stakeAddress = stakeCredential, amount = amount, redeemer = Nothing } :: preProcessedIntents.withdrawals
                                , nativeScriptSources = script :: preProcessedIntents.nativeScriptSources
                            }

                        Just (PlutusWitness { script, redeemerData, requiredSigners }) ->
                            { preProcessedIntents
                                | withdrawals = { stakeAddress = stakeCredential, amount = amount, redeemer = Just redeemerData } :: preProcessedIntents.withdrawals
                                , plutusScriptSources = script :: preProcessedIntents.plutusScriptSources
                                , requiredSigners = requiredSigners :: preProcessedIntents.requiredSigners
                            }

                -- TODO: Handle certificates
                _ ->
                    Debug.todo "certificates"
    in
    -- Use fold right so that the outputs list is in the correct order
    List.foldr stepIntent noIntent txIntents


type alias ProcessedIntents =
    { freeInputs : Address.Dict Value
    , freeOutputs : Address.Dict Value
    , preSelected : { sum : Value, inputs : Utxo.RefDict (Maybe (InputsOutputs -> Data)) }
    , preCreated : InputsOutputs -> { sum : Value, outputs : List Output }
    , nativeScriptSources : List (WitnessSource NativeScript)
    , plutusScriptSources : List (WitnessSource PlutusScript)
    , datumSources : List (WitnessSource Data)
    , requiredSigners : List (Bytes CredentialHash)
    , totalMinted : MultiAsset Integer
    , mintRedeemers : BytesMap PolicyId (Maybe (InputsOutputs -> Data))
    , withdrawals : Address.StakeDict { amount : Natural, redeemer : Maybe (InputsOutputs -> Data) }
    }


type TxIntentError
    = TxIntentError String


{-| Process already pre-processed intents and validate them all.
-}
processIntents : Utxo.RefDict Output -> List TxIntent -> Result TxFinalizationError ProcessedIntents
processIntents localStateUtxos txIntents =
    let
        preProcessedIntents =
            preProcessIntents txIntents

        -- Accumulate all output references from inputs and witnesses.
        allOutputReferencesInIntents : Utxo.RefDict ()
        allOutputReferencesInIntents =
            List.concat
                [ List.map .input preProcessedIntents.preSelected
                , List.filterMap extractWitnessRef preProcessedIntents.nativeScriptSources
                , List.filterMap extractWitnessRef preProcessedIntents.plutusScriptSources
                , List.filterMap extractWitnessRef preProcessedIntents.datumSources
                ]
                |> List.map (\ref -> ( ref, () ))
                |> Utxo.refDictFromList

        -- Check that all referenced inputs are present in the local state
        absentOutputReferencesInLocalState : Utxo.RefDict ()
        absentOutputReferencesInLocalState =
            Dict.Any.diff allOutputReferencesInIntents
                (Dict.Any.map (\_ _ -> ()) localStateUtxos)

        totalMintedAndBurned : MultiAsset Integer
        totalMintedAndBurned =
            List.map (\m -> Map.singleton m.policyId m.assets) preProcessedIntents.mints
                |> List.foldl MultiAsset.mintAdd MultiAsset.empty
                |> MultiAsset.normalize Integer.isZero

        -- Extract total minted value and total burned value
        splitMintsBurns =
            List.map (\m -> ( m.policyId, MultiAsset.balance m.assets )) preProcessedIntents.mints

        totalMintedValue =
            List.foldl (\( p, { minted } ) -> Value.addTokens (Map.singleton p minted)) Value.zero splitMintsBurns

        totalBurnedValue =
            List.foldl (\( p, { burned } ) -> Value.addTokens (Map.singleton p burned)) Value.zero splitMintsBurns

        -- Extract total ada amount withdrawn
        totalWithdrawalAmount =
            List.foldl (\w acc -> Natural.add w.amount acc) Natural.zero preProcessedIntents.withdrawals

        -- Retrieve the ada and tokens amount at a given output reference
        getValueFromRef : OutputReference -> Value
        getValueFromRef ref =
            Dict.Any.get ref localStateUtxos
                |> Maybe.map .amount
                |> Maybe.withDefault Value.zero

        -- Extract value thanks to input refs
        -- Also add minted tokens and withdrawals to preSelected
        preSelected =
            preProcessedIntents.preSelected
                |> List.foldl (\s -> addPreSelectedInput s.input (getValueFromRef s.input) s.redeemer)
                    { sum = Value.add totalMintedValue (Value.onlyLovelace totalWithdrawalAmount)
                    , inputs = Utxo.emptyRefDict
                    }

        -- Add burned tokens to preCreated
        preCreated =
            \inputsOutputs ->
                let
                    { sum, outputs } =
                        preProcessedIntents.preCreated inputsOutputs
                in
                { sum = Value.add sum totalBurnedValue, outputs = outputs }

        preCreatedOutputs =
            preCreated noInputsOutputs

        -- Compute total inputs and outputs to check the Tx balance
        totalInput =
            Dict.Any.foldl (\_ -> Value.add) preSelected.sum preProcessedIntents.freeInputs

        totalOutput =
            Dict.Any.foldl (\_ -> Value.add) preCreatedOutputs.sum preProcessedIntents.freeOutputs
    in
    if not <| Dict.Any.isEmpty absentOutputReferencesInLocalState then
        Err <| ReferenceOutputsMissingFromLocalState (Dict.Any.keys absentOutputReferencesInLocalState)

    else if totalInput /= totalOutput then
        let
            _ =
                Debug.log "totalInput" totalInput

            _ =
                Debug.log "totalOutput" totalOutput
        in
        Err <| UnbalancedIntents "Tx is not balanced.\n"

    else
        validMinAdaPerOutput preCreatedOutputs.outputs
            |> Result.mapError NotEnoughMinAda
            |> Result.map
                (\_ ->
                    let
                        -- Dedup required signers
                        requiredSigners =
                            List.concat preProcessedIntents.requiredSigners
                                |> List.map (\signer -> ( signer, () ))
                                |> Map.fromList
                                |> Map.keys
                    in
                    { freeInputs = preProcessedIntents.freeInputs
                    , freeOutputs = preProcessedIntents.freeOutputs
                    , preSelected = preSelected
                    , preCreated = preCreated
                    , nativeScriptSources = dedupWitnessSources Script.encodeNativeScript preProcessedIntents.nativeScriptSources
                    , plutusScriptSources = dedupWitnessSources Script.encodePlutusScript preProcessedIntents.plutusScriptSources
                    , datumSources = dedupWitnessSources Data.toCbor preProcessedIntents.datumSources
                    , requiredSigners = requiredSigners
                    , totalMinted = totalMintedAndBurned
                    , mintRedeemers =
                        List.map (\m -> ( m.policyId, m.redeemer )) preProcessedIntents.mints
                            |> Map.fromList
                    , withdrawals =
                        List.map (\w -> ( w.stakeAddress, { amount = w.amount, redeemer = w.redeemer } )) preProcessedIntents.withdrawals
                            |> Address.stakeDictFromList
                    }
                )


{-| Helper function
-}
dedupWitnessSources : (a -> E.Encoder) -> List (WitnessSource a) -> List (WitnessSource a)
dedupWitnessSources toCbor sources =
    let
        -- Split values and references in two lists
        ( values, refs ) =
            List.foldl
                (\source ( vs, rs ) ->
                    case source of
                        WitnessValue v ->
                            ( v :: vs, rs )

                        WitnessReference ref ->
                            ( vs, ref :: rs )
                )
                ( [], [] )
                sources

        -- Create the comparable function from the encoder
        toComparable v =
            E.encode (toCbor v) |> Bytes.fromBytes |> Bytes.toString

        -- Dedup values
        dedupedValues =
            List.map (\v -> ( v, () )) values
                |> Dict.Any.fromList toComparable
                |> Dict.Any.keys

        dedupedRefs =
            List.map (\ref -> ( ref, () )) refs
                |> Utxo.refDictFromList
                |> Dict.Any.keys
    in
    List.map WitnessValue dedupedValues ++ List.map WitnessReference dedupedRefs


{-| Helper function
-}
addPreSelectedInput :
    OutputReference
    -> Value
    -> Maybe (InputsOutputs -> Data)
    -> { sum : Value, inputs : Utxo.RefDict (Maybe (InputsOutputs -> Data)) }
    -> { sum : Value, inputs : Utxo.RefDict (Maybe (InputsOutputs -> Data)) }
addPreSelectedInput ref value maybeRedeemer { sum, inputs } =
    { sum = Value.add value sum
    , inputs = Dict.Any.insert ref maybeRedeemer inputs
    }


validMinAdaPerOutput : List Output -> Result String ()
validMinAdaPerOutput outputs =
    case outputs of
        [] ->
            Ok ()

        output :: rest ->
            case Utxo.checkMinAda output of
                Ok _ ->
                    validMinAdaPerOutput rest

                Err err ->
                    Err err


type alias ProcessedOtherInfo =
    { referenceInputs : List OutputReference
    , metadata : List { tag : Natural, metadata : Metadatum }
    , timeValidityRange : Maybe { start : Int, end : Natural }
    }


noInfo : ProcessedOtherInfo
noInfo =
    { referenceInputs = []
    , metadata = []
    , timeValidityRange = Nothing
    }


type TxOtherInfoError
    = TxOtherInfoError String


processOtherInfo : List TxOtherInfo -> Result TxFinalizationError ProcessedOtherInfo
processOtherInfo otherInfo =
    let
        processedOtherInfo =
            List.foldl
                (\info acc ->
                    case info of
                        TxReferenceInput ref ->
                            { acc | referenceInputs = ref :: acc.referenceInputs }

                        TxMetadata m ->
                            { acc | metadata = m :: acc.metadata }

                        TxTimeValidityRange ({ start, end } as newVR) ->
                            { acc
                                | timeValidityRange =
                                    case acc.timeValidityRange of
                                        Nothing ->
                                            Just newVR

                                        Just vr ->
                                            Just { start = max start vr.start, end = Natural.min end vr.end }
                            }
                )
                noInfo
                otherInfo

        -- Check if there are duplicate metadata tags.
        -- (use Int instead of Natural for this purpose)
        metadataTags =
            List.map (.tag >> Natural.toInt) processedOtherInfo.metadata

        hasDuplicatedMetadataTags =
            List.length metadataTags /= Set.size (Set.fromList metadataTags)

        -- Check that the time range intersection is still valid
        validTimeRange =
            case processedOtherInfo.timeValidityRange of
                Nothing ->
                    True

                Just range ->
                    Natural.fromSafeInt range.start |> Natural.isLessThan range.end
    in
    if hasDuplicatedMetadataTags then
        let
            findDuplicate current tags =
                case tags of
                    [] ->
                        Nothing

                    t :: biggerTags ->
                        if t == current then
                            Just t

                        else
                            findDuplicate t biggerTags

            dupTag =
                findDuplicate -1 (List.sort metadataTags)
                    |> Maybe.withDefault -1
        in
        Err <| DuplicatedMetadataTags dupTag

    else if not validTimeRange then
        Err <| IncorrectTimeValidityRange <| "Invalid time range (or intersection of multiple time ranges). The time range end must be > than the start." ++ Debug.toString processedOtherInfo.timeValidityRange

    else
        Ok processedOtherInfo


{-| Perform collateral selection.

Only UTxOs at the provided whitelist of addresses are viable.
Only UTxOs containing only Ada, without other CNT or datums are viable.

-}
computeCollateralSelection :
    Utxo.RefDict Output
    -> Address.Dict ()
    -> Natural
    -> Result TxFinalizationError CoinSelection.Selection
computeCollateralSelection localStateUtxos collateralSources collateralAmount =
    CoinSelection.largestFirst 10
        { alreadySelectedUtxos = []
        , targetAmount = Value.onlyLovelace collateralAmount
        , availableUtxos =
            Dict.Any.toList localStateUtxos
                |> List.filter
                    (\( _, output ) ->
                        Utxo.isAdaOnly output
                            && Dict.Any.member output.address collateralSources
                    )
        }
        |> Result.mapError CollateralSelectionError


{-| Perform coin selection for the required input per address.

For each address, create an [Output] with the change.
The output must satisfy minAda.

TODO: If there is more than 5 ada free in the change (after minAda),
also create a pure-ada output so that we don’t deplete all outputs viable for collateral.

-}
computeCoinSelection :
    Utxo.RefDict Output
    -> Fee
    -> ProcessedIntents
    -> CoinSelection.Algorithm
    -> Result TxFinalizationError (Address.Dict ( CoinSelection.Selection, List Output ))
computeCoinSelection localStateUtxos fee processedIntents coinSelectionAlgo =
    let
        dummyOutput =
            { address = Byron <| Bytes.fromStringUnchecked ""
            , amount = Value.zero
            , datumOption = Nothing
            , referenceScript = Nothing
            }

        -- Inputs not available for selection because already manually preselected
        notAvailableInputs =
            -- Using dummyOutput to have the same type as localStateUtxos
            Dict.Any.map (\_ _ -> dummyOutput) processedIntents.preSelected.inputs

        -- Precompute selectable inputs per addresses
        availableInputs : Address.Dict (List ( OutputReference, Output ))
        availableInputs =
            Dict.Any.diff localStateUtxos notAvailableInputs
                --> Utxo.RefDict Output
                |> Dict.Any.foldl
                    (\ref output ->
                        -- append the output to the list of outputs for the same address
                        Dict.Any.update output.address
                            (Just << (::) ( ref, output ) << Maybe.withDefault [])
                    )
                    Address.emptyDict

        -- TODO: adjust at least with the number of different tokens in target Amount
        maxInputCount =
            10

        -- Add the fee to free inputs
        addFee : Address -> Natural -> Address.Dict Value -> Address.Dict Value
        addFee addr amount dict =
            Dict.Any.update addr (Just << Value.add (Value.onlyLovelace amount) << Maybe.withDefault Value.zero) dict

        freeInputsWithFee : Address.Dict Value
        freeInputsWithFee =
            case fee of
                ManualFee perAddressFee ->
                    List.foldl
                        (\{ paymentSource, exactFeeAmount } -> addFee paymentSource exactFeeAmount)
                        processedIntents.freeInputs
                        perAddressFee

                AutoFee { paymentSource } ->
                    addFee paymentSource defaultAutoFee processedIntents.freeInputs

        -- These are the free outputs that are unrelated to any address with fees or free input.
        -- It’s address dict keys are all different from those of freeInputsWithFee
        independentFreeOutputValues : Address.Dict Value
        independentFreeOutputValues =
            Dict.Any.diff processedIntents.freeOutputs freeInputsWithFee

        -- These will require they have enough minAda to make their own independent outputs.
        validIndependentFreeOutputs : Result TxFinalizationError (Address.Dict Output)
        validIndependentFreeOutputs =
            independentFreeOutputValues
                |> Dict.Any.map (\addr output -> Utxo.checkMinAda <| Utxo.simpleOutput addr output)
                |> resultDictJoin
                |> Result.mapError NotEnoughMinAda

        -- These are the free outputs that are related to any address with fees or free input.
        -- It’s address dict keys are a subset of those of freeInputsWithFee
        relatedFreeOutputValues : Address.Dict Value
        relatedFreeOutputValues =
            Dict.Any.diff processedIntents.freeOutputs independentFreeOutputValues

        -- Merge the two dicts :
        --   - freeInputsWithFee (that will become the coin selection target value)
        --   - relatedFreeOutputValues (that will be added to the coin selection change)
        targetValuesAndOutputs : Address.Dict { targetInputValue : Value, freeOutput : Value }
        targetValuesAndOutputs =
            let
                whenInput addr v =
                    Dict.Any.insert addr { targetInputValue = v, freeOutput = Value.zero }

                whenOutput addr v =
                    Dict.Any.insert addr { targetInputValue = Value.zero, freeOutput = v }

                whenBoth addr input output =
                    Dict.Any.insert addr { targetInputValue = input, freeOutput = output }
            in
            Dict.Any.merge whenInput
                whenBoth
                whenOutput
                freeInputsWithFee
                relatedFreeOutputValues
                Address.emptyDict

        -- Perform coin selection and output creation with the change
        -- for all address where there are target values (inputs and fees)
        coinSelectionAndChangeOutputs : Result TxFinalizationError (Address.Dict ( CoinSelection.Selection, List Output ))
        coinSelectionAndChangeOutputs =
            targetValuesAndOutputs
                -- Apply the selection algo for each address with input requirements
                |> Dict.Any.map
                    (\addr { targetInputValue, freeOutput } ->
                        let
                            hasFreeOutput =
                                freeOutput /= Value.zero

                            availableUtxosDict =
                                Maybe.withDefault [] (Dict.Any.get addr availableInputs)
                                    |> Utxo.refDictFromList

                            context targetAmount alreadySelected =
                                { targetAmount = targetAmount
                                , alreadySelectedUtxos = alreadySelected
                                , availableUtxos =
                                    Dict.Any.diff availableUtxosDict (Utxo.refDictFromList alreadySelected)
                                        |> Dict.Any.toList
                                }

                            -- Create the output(s) with the change + free output, if there is enough minAda
                            makeChangeOutput : CoinSelection.Selection -> Result CoinSelection.Error ( CoinSelection.Selection, List Output )
                            makeChangeOutput selection =
                                case ( selection.change, hasFreeOutput ) of
                                    ( Nothing, False ) ->
                                        Ok ( selection, [] )

                                    _ ->
                                        let
                                            change =
                                                Value.add (Maybe.withDefault Value.zero selection.change) freeOutput

                                            changeOutput =
                                                { address = addr
                                                , amount = change
                                                , datumOption = Nothing
                                                , referenceScript = Nothing
                                                }

                                            minAda =
                                                Utxo.minAda changeOutput
                                        in
                                        if change.lovelace |> Natural.isGreaterThanOrEqual minAda then
                                            -- TODO: later, if there is more than 5 free ada, make an additional ada-only output
                                            Ok ( selection, [ changeOutput ] )

                                        else
                                            Err <|
                                                CoinSelection.UTxOBalanceInsufficient
                                                    { selectedUtxos = selection.selectedUtxos
                                                    , missingValue = Value.onlyLovelace <| Natural.sub minAda change.lovelace
                                                    }

                            coinSelectIter targetValue alreadySelected =
                                coinSelectionAlgo maxInputCount (context targetValue alreadySelected)
                                    |> Result.andThen makeChangeOutput
                        in
                        -- Try coin selection up to 2 times if the only missing value is Ada.
                        -- Why 2 times? because the first time, it might be missing minAda for the change output.
                        case coinSelectIter targetInputValue [] of
                            (Err (CoinSelection.UTxOBalanceInsufficient err1)) as err ->
                                if MultiAsset.isEmpty err1.missingValue.assets then
                                    coinSelectIter (Value.add targetInputValue err1.missingValue) err1.selectedUtxos

                                else
                                    err

                            selectionResult ->
                                selectionResult
                    )
                -- Join the Dict (Result _ _) into Result _ Dict
                |> resultDictJoin
                |> Result.mapError FailedToPerformCoinSelection
    in
    Result.map2
        (Dict.Any.foldl (\addr output -> Dict.Any.insert addr ( { selectedUtxos = [], change = Nothing }, [ output ] )))
        coinSelectionAndChangeOutputs
        validIndependentFreeOutputs


{-| Helper function to join Dict Result into Result Dict.
-}
resultDictJoin : AnyDict comparable key (Result err value) -> Result err (AnyDict comparable key value)
resultDictJoin dict =
    Dict.Any.foldl (\key -> Result.map2 (Dict.Any.insert key)) (Ok <| Dict.Any.removeAll dict) dict


{-| Helper function to accumulate all selected UTxOs and newly created outputs.
-}
accumPerAddressSelection :
    Address.Dict ( CoinSelection.Selection, List Output )
    -> { selectedInputs : Utxo.RefDict Output, createdOutputs : List Output }
accumPerAddressSelection allSelections =
    Dict.Any.foldl
        (\addr ( { selectedUtxos }, createdOutputs ) acc ->
            { selectedInputs =
                List.foldl (\( ref, output ) -> Dict.Any.insert ref output) acc.selectedInputs selectedUtxos
            , createdOutputs = createdOutputs ++ acc.createdOutputs
            }
        )
        { selectedInputs = Utxo.emptyRefDict, createdOutputs = [] }
        allSelections


{-| Helper function to update Tx inputs/outputs after coin selection.
-}
updateInputsOutputs : ProcessedIntents -> { selectedInputs : Utxo.RefDict Output, createdOutputs : List Output } -> InputsOutputs -> InputsOutputs
updateInputsOutputs intents { selectedInputs, createdOutputs } old =
    -- reference inputs do not change with UTxO selection, only spent inputs
    { referenceInputs = old.referenceInputs
    , spentInputs =
        let
            preSelected : Utxo.RefDict ()
            preSelected =
                Dict.Any.map (\_ _ -> ()) intents.preSelected.inputs

            algoSelected : Utxo.RefDict ()
            algoSelected =
                Dict.Any.map (\_ _ -> ()) selectedInputs
        in
        Dict.Any.keys (Dict.Any.union preSelected algoSelected)
    , createdOutputs = (intents.preCreated old).outputs ++ createdOutputs
    }


{-| Build the Transaction from the processed intents and the latest inputs/outputs.
-}
buildTx :
    Utxo.RefDict Output
    -> Natural
    -> CoinSelection.Selection
    -> ProcessedIntents
    -> ProcessedOtherInfo
    -> InputsOutputs
    -> Transaction
buildTx localStateUtxos feeAmount collateralSelection processedIntents otherInfo inputsOutputs =
    let
        -- WitnessSet ######################################
        --
        ( nativeScripts, nativeScriptRefs ) =
            splitWitnessSources processedIntents.nativeScriptSources

        ( plutusScripts, plutusScriptRefs ) =
            splitWitnessSources processedIntents.plutusScriptSources

        ( datumWitnessValues, datumWitnessRefs ) =
            splitWitnessSources processedIntents.datumSources

        -- Compute datums for pre-selected inputs.
        preSelected : Utxo.RefDict (Maybe Data)
        preSelected =
            processedIntents.preSelected.inputs
                |> Dict.Any.map (\_ -> Maybe.map (\f -> f inputsOutputs))

        -- Add a default Nothing to all inputs picked by the selection algorithm.
        algoSelected : Utxo.RefDict (Maybe Data)
        algoSelected =
            List.map (\ref -> ( ref, Nothing )) inputsOutputs.spentInputs
                |> Utxo.refDictFromList

        -- Helper
        makeRedeemer : RedeemerTag -> Int -> Data -> Redeemer
        makeRedeemer tag id data =
            { tag = tag
            , index = id
            , data = data
            , exUnits = { mem = 0, steps = 0 } -- TODO: change or not?
            }

        -- Build the spend redeemers while keeping the index of the sorted inputs.
        sortedSpendRedeemers : List Redeemer
        sortedSpendRedeemers =
            Dict.Any.diff algoSelected preSelected
                -- The diff then union is to make sure the order does not matter
                -- since we want to keep the Just Data of preSelected
                -- insteaf of the Nothings of algoSelected
                |> Dict.Any.union preSelected
                |> Dict.Any.toList
                |> List.indexedMap
                    (\id ( _, maybeDatum ) ->
                        Maybe.map (makeRedeemer Redeemer.Spend id) maybeDatum
                    )
                |> List.filterMap identity

        -- Build the mint redeemers while keeping the index of the sorted order of policy IDs.
        sortedMintRedeemers : List Redeemer
        sortedMintRedeemers =
            Map.values processedIntents.mintRedeemers
                |> List.indexedMap
                    (\id maybeRedeemerF ->
                        Maybe.map
                            (\redeemerF -> makeRedeemer Redeemer.Mint id (redeemerF inputsOutputs))
                            maybeRedeemerF
                    )
                |> List.filterMap identity

        sortedWithdrawals : List ( StakeAddress, Natural, Maybe Data )
        sortedWithdrawals =
            Dict.Any.toList processedIntents.withdrawals
                |> List.map (\( addr, w ) -> ( addr, w.amount, Maybe.map (\f -> f inputsOutputs) w.redeemer ))

        -- Build the withdrawals redeemers while keeping the index in the sorted list.
        sortedWithdrawalsRedeemers : List Redeemer
        sortedWithdrawalsRedeemers =
            sortedWithdrawals
                |> List.indexedMap
                    (\id ( _, _, maybeDatum ) ->
                        Maybe.map (makeRedeemer Redeemer.Reward id) maybeDatum
                    )
                |> List.filterMap identity

        -- TODO
        sortedCertRedeemers : List Redeemer
        sortedCertRedeemers =
            []

        -- Look for inputs at addresses that will need signatures
        walletCredsInInputs : List (Bytes CredentialHash)
        walletCredsInInputs =
            inputsOutputs.spentInputs
                |> List.filterMap
                    (\ref ->
                        Dict.Any.get ref localStateUtxos
                            |> Maybe.andThen (Address.extractPubKeyHash << .address)
                    )

        -- Create a dummy VKey Witness for each input wallet address or required signer
        -- so that fees are correctly estimated.
        dummyVKeyWitness : List VKeyWitness
        dummyVKeyWitness =
            (walletCredsInInputs ++ processedIntents.requiredSigners)
                |> List.map (\cred -> ( cred, { vkey = dummyBytes 32, signature = dummyBytes 64 } ))
                -- Convert to a BytesMap to ensure credentials unicity
                |> Map.fromList
                |> Map.values

        txWitnessSet : WitnessSet
        txWitnessSet =
            { vkeywitness = nothingIfEmptyList dummyVKeyWitness
            , bootstrapWitness = Nothing
            , plutusData = nothingIfEmptyList datumWitnessValues
            , nativeScripts = nothingIfEmptyList nativeScripts
            , plutusV1Script = nothingIfEmptyList <| filterScriptVersion PlutusV1 plutusScripts
            , plutusV2Script = nothingIfEmptyList <| filterScriptVersion PlutusV2 plutusScripts
            , plutusV3Script = nothingIfEmptyList <| filterScriptVersion PlutusV3 plutusScripts
            , redeemer =
                nothingIfEmptyList <|
                    List.concat
                        [ sortedSpendRedeemers
                        , sortedMintRedeemers
                        , sortedWithdrawalsRedeemers
                        , sortedCertRedeemers
                        ]
            }

        -- AuxiliaryData ###################################
        --
        txAuxData : Maybe AuxiliaryData
        txAuxData =
            case otherInfo.metadata of
                [] ->
                    Nothing

                _ ->
                    Just
                        { labels = List.map (\{ tag, metadata } -> ( tag, metadata )) otherInfo.metadata
                        , nativeScripts = []
                        , plutusV1Scripts = []
                        , plutusV2Scripts = []
                        , plutusV3Scripts = []
                        }

        -- TransactionBody #################################
        --
        -- Regroup all OutputReferences from witnesses
        allReferenceInputs =
            List.concat
                [ inputsOutputs.referenceInputs
                , otherInfo.referenceInputs
                , nativeScriptRefs
                , plutusScriptRefs
                , datumWitnessRefs
                ]
                |> List.map (\ref -> ( ref, () ))
                |> Utxo.refDictFromList
                |> Dict.Any.keys

        -- Helper function to create dummy bytes, mostly for fee estimation
        dummyBytes bytesLength =
            Bytes.fromStringUnchecked (String.repeat (2 * bytesLength) "0")

        -- Script data is serialized in a very specific way to compute the hash.
        -- See Conway CDDL format: https://github.com/IntersectMBO/cardano-ledger/blob/676ffc5c3e0dddb2b1ddeb76627541b195fefb5a/eras/conway/impl/cddl-files/conway.cddl#L197
        -- See Blaze impl: https://github.com/butaneprotocol/blaze-cardano/blob/1c9c603755e5d48b6bf91ea086d6231d6d8e76df/packages/blaze-tx/src/tx.ts#L935
        -- See cardano-js-sdk serialization of redeemers: https://github.com/input-output-hk/cardano-js-sdk/blob/0d138c98ccf7ad15a495f02e4a50d84f661a9d38/packages/core/src/Serialization/TransactionWitnessSet/Redeemer/Redeemers.ts#L29
        scriptDataHash : Maybe (Bytes ScriptDataHash)
        scriptDataHash =
            if txWitnessSet.redeemer == Nothing && txWitnessSet.plutusData == Nothing then
                Nothing

            else
                -- TODO: actual hashing
                Just (dummyBytes 32)

        collateralReturnAmount =
            (Maybe.withDefault Value.zero collateralSelection.change).lovelace

        collateralReturn : Maybe Output
        collateralReturn =
            List.head collateralSelection.selectedUtxos
                |> Maybe.map (\( _, output ) -> Utxo.fromLovelace output.address collateralReturnAmount)

        totalCollateral : Maybe Int
        totalCollateral =
            if List.isEmpty collateralSelection.selectedUtxos then
                Nothing

            else
                collateralSelection.selectedUtxos
                    |> List.foldl (\( _, o ) -> Natural.add o.amount.lovelace) Natural.zero
                    |> Natural.toInt
                    |> Just

        txBody : TransactionBody
        txBody =
            { inputs = inputsOutputs.spentInputs
            , outputs = inputsOutputs.createdOutputs
            , fee = Just feeAmount
            , ttl = Maybe.map .end otherInfo.timeValidityRange
            , certificates = [] -- TODO
            , withdrawals = List.map (\( addr, amount, _ ) -> ( addr, amount )) sortedWithdrawals
            , update = Nothing
            , auxiliaryDataHash =
                case otherInfo.metadata of
                    [] ->
                        Nothing

                    _ ->
                        -- TODO: compute actual auxiliary data hash
                        Just (dummyBytes 32)
            , validityIntervalStart = Maybe.map .start otherInfo.timeValidityRange
            , mint = processedIntents.totalMinted
            , scriptDataHash = scriptDataHash
            , collateral = List.map Tuple.first collateralSelection.selectedUtxos
            , requiredSigners = processedIntents.requiredSigners
            , networkId = Nothing -- not mandatory
            , collateralReturn = collateralReturn
            , totalCollateral = totalCollateral
            , referenceInputs = allReferenceInputs
            , votingProcedures = [] -- TODO votingProcedures
            , proposalProcedures = [] -- TODO proposalProcedures
            , currentTreasuryValue = Nothing -- TODO currentTreasuryValue
            , treasuryDonation = Nothing -- TODO treasuryDonation
            }
    in
    { body = txBody
    , witnessSet = txWitnessSet
    , isValid = True
    , auxiliaryData = txAuxData
    }


{-| Helper function to split native script into a list of script value and a list of output references.
-}
splitWitnessSources : List (WitnessSource a) -> ( List a, List OutputReference )
splitWitnessSources witnessSources =
    List.foldl
        (\w ( accValues, accRefs ) ->
            case w of
                WitnessValue value ->
                    ( value :: accValues, accRefs )

                WitnessReference ref ->
                    ( accValues, ref :: accRefs )
        )
        ( [], [] )
        witnessSources


{-| Helper
-}
nothingIfEmptyList : List a -> Maybe (List a)
nothingIfEmptyList list =
    if List.isEmpty list then
        Nothing

    else
        Just list


{-| Helper
-}
filterScriptVersion : Script.PlutusVersion -> List PlutusScript -> List (Bytes ScriptCbor)
filterScriptVersion v =
    List.filterMap
        (\{ version, script } ->
            if version == v then
                Just script

            else
                Nothing
        )


{-| Adjust the steps/mem scripts execution costs with UPLC phase 2 evaluation of the transaction.
-}
adjustExecutionCosts : (Transaction -> Result String (List Redeemer)) -> Transaction -> Result TxFinalizationError Transaction
adjustExecutionCosts evalScriptsCosts tx =
    evalScriptsCosts tx
        |> Result.mapError UplcVmError
        |> Result.map
            (\redeemers ->
                if List.isEmpty redeemers then
                    tx

                else
                    let
                        witnessSet =
                            tx.witnessSet
                    in
                    { tx | witnessSet = { witnessSet | redeemer = Just redeemers } }
            )


{-| Final check for the Tx fees.
-}
checkInsufficientFee : Fee -> Transaction -> Result TxFinalizationError Transaction
checkInsufficientFee fee tx =
    let
        declaredFee =
            Maybe.withDefault Natural.zero tx.body.fee

        computedFee =
            Transaction.computeFees tx
    in
    if declaredFee |> Natural.isLessThan computedFee then
        case fee of
            ManualFee perAddressFee ->
                Err <| InsufficientManualFee { declared = declaredFee, computed = computedFee }

            AutoFee _ ->
                Err <| FailurePleaseReportToElmCardano "Insufficient AutoFee. Maybe we need another buildTx round?"

    else
        Ok tx



-- EXAMPLES ##########################################################


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


prettyAddr address =
    case address of
        Byron b ->
            (Bytes.toText >> Maybe.withDefault "") b

        Shelley { paymentCredential, stakeCredential } ->
            [ Just "Addr:", Just (prettyCred paymentCredential), Maybe.map prettyStakeCred stakeCredential ]
                |> List.filterMap identity
                |> String.join " "

        Reward stakeAddr ->
            "StakeAddr:" ++ prettyCred stakeAddr.stakeCredential


prettyStakeCred stakeCred =
    case stakeCred of
        Address.InlineCredential cred ->
            "stake:" ++ prettyCred cred

        Address.PointerCredential _ ->
            "stake:PointerAddr"


prettyCred cred =
    case cred of
        Address.VKeyHash b ->
            "key:" ++ (Bytes.toText >> Maybe.withDefault "") b

        Address.ScriptHash b ->
            "script:" ++ (Bytes.toText >> Maybe.withDefault "") b


prettyWithdrawal : ( StakeAddress, Natural ) -> String
prettyWithdrawal ( { stakeCredential }, amount ) =
    "₳ " ++ Natural.toString amount ++ " @ stakeCred:" ++ prettyCred stakeCredential


prettyValue : Value -> List String
prettyValue { lovelace, assets } =
    if MultiAsset.isEmpty assets then
        [ "₳ " ++ Natural.toString lovelace ]

    else
        "with native assets:"
            :: ("   ₳ " ++ Natural.toString lovelace)
            :: List.map (indent 3) (prettyAssets Natural.toString assets)


prettyAssets toStr multiAsset =
    Map.toList multiAsset
        |> List.concatMap
            (\( policyId, assets ) ->
                Map.toList assets
                    |> List.map
                        (\( name, amount ) ->
                            String.join " "
                                [ (Bytes.toText >> Maybe.withDefault "") policyId
                                , (Bytes.toText >> Maybe.withDefault "") name
                                , toStr amount
                                ]
                        )
            )


prettyDatum datumOption =
    case datumOption of
        Utxo.DatumHash h ->
            "datumHash: " ++ Maybe.withDefault "" (Bytes.toText h)

        Utxo.Datum data ->
            "datum: " ++ prettyCbor Data.toCbor data


prettyCbor toCbor x =
    E.encode (toCbor x) |> Bytes.fromBytes |> Bytes.toString


prettyScript script =
    case script of
        Script.Native nativeScript ->
            "NativeScript: " ++ prettyCbor Script.encodeNativeScript nativeScript

        Script.Plutus plutusScript ->
            "PlutusScript: " ++ prettyCbor Script.encodePlutusScript plutusScript


prettyInput ref =
    String.join " "
        [ "TxId:" ++ (Bytes.toText >> Maybe.withDefault "") ref.transactionId
        , "#" ++ String.fromInt ref.outputIndex
        ]


prettyOutput : Output -> List String
prettyOutput { address, amount, datumOption, referenceScript } =
    ("- " ++ prettyAddr address)
        :: ([ Just <| prettyValue amount
            , Maybe.map (List.singleton << prettyDatum) datumOption
            , Maybe.map (List.singleton << prettyScript) referenceScript
            ]
                |> List.filterMap identity
                |> List.concat
                |> List.map (indent 2)
           )


prettyList sectionTitle prettify list =
    if List.isEmpty list then
        []

    else
        sectionTitle
            :: List.map (indent 3 << prettify) list


prettyMints sectionTitle multiAsset =
    if MultiAsset.isEmpty multiAsset then
        []

    else
        sectionTitle
            :: List.map (indent 3) (prettyAssets Integer.toString multiAsset)


prettyVKeyWitness { vkey, signature } =
    String.join ", "
        [ "vkey:" ++ Bytes.toString vkey
        , "signature:" ++ Bytes.toString signature
        ]


prettyRedeemer redeemer =
    String.join " "
        [ Debug.toString redeemer.tag
        , "index:" ++ String.fromInt redeemer.index
        , "exUnits:?"
        , "data:" ++ prettyCbor Data.toCbor redeemer.data
        ]


prettyMetadata ( tag, metadatum ) =
    Natural.toString tag ++ ": " ++ prettyCbor Metadatum.toCbor metadatum


indent spaces str =
    String.repeat spaces " " ++ str


prettyTx : Transaction -> String
prettyTx tx =
    let
        prettyBytes b =
            Maybe.withDefault (Bytes.toString b) (Bytes.toText b)

        body =
            List.concat
                [ [ "Tx fee: ₳ " ++ (Maybe.withDefault Natural.zero tx.body.fee |> Natural.toString) ]
                , prettyList "Tx ref inputs:" prettyInput tx.body.referenceInputs
                , prettyList "Tx inputs:" prettyInput tx.body.inputs
                , [ "Tx outputs:" ]
                , List.concatMap prettyOutput tx.body.outputs
                    |> List.map (indent 3)
                , prettyMints "Tx mints:" tx.body.mint
                , prettyList "Tx withdrawals:" prettyWithdrawal tx.body.withdrawals
                , prettyList "Tx required signers:" prettyBytes tx.body.requiredSigners
                , prettyList
                    ("Tx collateral (total: ₳ " ++ (Maybe.withDefault "not set" <| Maybe.map String.fromInt tx.body.totalCollateral) ++ "):")
                    prettyInput
                    tx.body.collateral
                , Maybe.map prettyOutput tx.body.collateralReturn
                    |> Maybe.withDefault []
                    |> prettyList "Tx collateral return:" identity
                ]

        witnessSet =
            List.concat <|
                List.filterMap identity
                    [ tx.witnessSet.vkeywitness
                        |> Maybe.map (prettyList "Tx vkey witness:" prettyVKeyWitness)
                    , tx.witnessSet.nativeScripts
                        |> Maybe.map (prettyList "Tx native scripts:" (prettyScript << Script.Native))
                    , tx.witnessSet.plutusV1Script
                        |> Maybe.map (prettyList "Tx plutus V1 scripts:" prettyBytes)
                    , tx.witnessSet.plutusV2Script
                        |> Maybe.map (prettyList "Tx plutus V2 scripts:" prettyBytes)
                    , tx.witnessSet.redeemer
                        |> Maybe.map (prettyList "Tx redeemers:" prettyRedeemer)
                    , Nothing -- TODO: plutusData
                    ]

        -- Pretty print auxiliary data
        auxiliaryData =
            case tx.auxiliaryData of
                Nothing ->
                    []

                Just auxData ->
                    List.concat <|
                        [ prettyList "Tx metadata:" prettyMetadata auxData.labels
                        , prettyList "Tx native scripts in auxiliary data:" (prettyScript << Script.Native) auxData.nativeScripts
                        , prettyList "Tx plutus V1 scripts in auxiliary data:" prettyBytes auxData.plutusV1Scripts
                        , prettyList "Tx plutus V2 scripts in auxiliary data:" prettyBytes auxData.plutusV2Scripts
                        ]
    in
    List.concat [ body, witnessSet, auxiliaryData ]
        |> String.join "\n"



-- EXAMPLES global state


ada =
    -- Asset amounts are typed with unbounded Natural numbers
    { one = Value.onlyLovelace (Natural.fromSafeString "1000000")
    , two = Value.onlyLovelace (Natural.fromSafeString "2000000")
    , ten = Value.onlyLovelace (Natural.fromSafeString "10000000")
    }


exAddr =
    { me = makeWalletAddress "me"
    , you = makeWalletAddress "you"
    }


dog =
    { policyId = Bytes.fromText "dog"
    , policyIdStr = "dog"
    , assetName = Bytes.fromText "yksoh"
    , assetNameStr = "yksoh"
    , scriptRef = makeRef "dogScriptRef" 0
    , refOutput =
        { address = makeAddress "dogScriptRefAddress"
        , amount = ada.two
        , datumOption = Nothing
        , referenceScript = Just <| Script.Native <| Script.ScriptAll [] -- dummy
        }
    }


cat =
    { policyId = Bytes.fromText "cat"
    , policyIdStr = "cat"
    , assetName = Bytes.fromText "felix"
    , assetNameStr = "felix"
    , scriptRef = makeRef "catScriptRef" 0
    , refOutput =
        { address = makeAddress "catScriptRefAddress"
        , amount = ada.two
        , datumOption = Nothing
        , referenceScript = Just <| Script.Native <| Script.ScriptAll [] -- dummy
        }
    }


globalStateUtxos : Utxo.RefDict Output
globalStateUtxos =
    Utxo.refDictFromList
        [ makeAdaOutput 0 exAddr.me 2 --   2 ada at my address
        , makeAdaOutput 1 exAddr.me 10 -- 10 ada at my address
        , makeAdaOutput 2 exAddr.me 5 --   5 ada at my address
        , makeAsset 3 exAddr.me dog.policyIdStr dog.assetNameStr 2
        , makeAsset 4 exAddr.me cat.policyIdStr cat.assetNameStr 5
        , ( dog.scriptRef, dog.refOutput )
        , ( cat.scriptRef, cat.refOutput )
        ]


defaultVmConfig =
    { budget = Uplc.conwayDefaultBudget
    , slotConfig = Uplc.slotConfigMainnet
    , costModels = Uplc.conwayDefaultCostModels
    }


globalConfig =
    { localStateUtxos = globalStateUtxos
    , coinSelectionAlgo = CoinSelection.largestFirst
    , evalScriptsCosts = Uplc.evalScriptsCosts defaultVmConfig
    }


globalConfigNoPlutus =
    { globalConfig | evalScriptsCosts = \_ _ -> Ok [] }


twoAdaFee =
    ManualFee [ { paymentSource = exAddr.me, exactFeeAmount = Natural.fromSafeInt 2000000 } ]


autoFee =
    AutoFee { paymentSource = exAddr.me }



-- EXAMPLE 1: Simple transfer


example1 _ =
    [ Spend <| From exAddr.me ada.one
    , SendTo exAddr.you ada.one
    ]
        |> finalize globalConfigNoPlutus autoFee [ TxMetadata { tag = Natural.fromSafeInt 14, metadata = Metadatum.Int (Integer.fromSafeInt 42) } ]



-- EXAMPLE 2: mint/burn with native script


example2 _ =
    -- minting 1 dog (amounts are of type Integer: unbounded positive or negative integers)
    [ MintBurn
        { policyId = dog.policyId
        , assets = Map.singleton dog.assetName Integer.one
        , scriptWitness = NativeWitness (WitnessReference dog.scriptRef)
        }
    , SendTo exAddr.me (Value.onlyToken dog.policyId dog.assetName Natural.one)

    -- burning 1 cat
    , Spend <| From exAddr.me (Value.onlyToken cat.policyId cat.assetName Natural.one)
    , MintBurn
        { policyId = cat.policyId
        , assets = Map.singleton cat.assetName Integer.negativeOne
        , scriptWitness = NativeWitness (WitnessReference cat.scriptRef)
        }
    ]
        |> finalize globalConfigNoPlutus autoFee []



-- EXAMPLE 3: spend from a Plutus script
-- The input index is provided in the redeemer


utxoBeingSpent =
    makeRef "previouslySentToLock" 0


findSpendingUtxo inputs =
    case inputs of
        [] ->
            0

        ( id, ref ) :: next ->
            if ref == utxoBeingSpent then
                id

            else
                findSpendingUtxo next


example3 _ =
    let
        ( myKeyCred, myStakeCred ) =
            ( Address.extractPubKeyHash exAddr.me
                |> Maybe.withDefault (Bytes.fromText "should not fail")
            , Address.extractStakeCredential exAddr.me
            )

        lock =
            { script = PlutusScript PlutusV2 (Bytes.fromText "LockScript")
            , scriptHash = Bytes.fromText "LockHash"
            }

        -- Combining the script hash with our stake credential
        -- to keep the locked add staked.
        lockScriptAddress =
            Address.Shelley
                { networkId = Mainnet
                , paymentCredential = ScriptHash lock.scriptHash
                , stakeCredential = myStakeCred
                }

        -- Build a redeemer that contains the index of the spent script input.
        redeemer inputsOutputs =
            List.indexedMap Tuple.pair inputsOutputs.spentInputs
                |> findSpendingUtxo
                |> (Data.Int << Integer.fromSafeInt)

        -- Helper function to create an output at the lock script address.
        -- It contains our key credential in the datum.
        makeLockedOutput adaAmount =
            { address = lockScriptAddress
            , amount = adaAmount
            , datumOption = Just (Datum (Data.Bytes <| Bytes.toAny myKeyCred))
            , referenceScript = Nothing
            }

        -- Add to local state utxos some previously sent 2 ada.
        localStateUtxos =
            globalConfig.localStateUtxos
                |> Dict.Any.insert utxoBeingSpent
                    (makeLockedOutput ada.two)
    in
    -- Collect 1 ada from the lock script
    [ Spend <|
        FromPlutusScript
            { spentInput = utxoBeingSpent
            , datumWitness = Nothing
            , plutusScriptWitness =
                { script = WitnessValue lock.script
                , redeemerData = redeemer
                , requiredSigners = [ myKeyCred ]
                }
            }
    , SendTo exAddr.me ada.one

    -- Return the other 1 ada to the lock script (there was 2 ada initially)
    , SendToOutput (\_ -> makeLockedOutput ada.one)
    ]
        |> finalize { globalConfig | localStateUtxos = localStateUtxos } autoFee []
