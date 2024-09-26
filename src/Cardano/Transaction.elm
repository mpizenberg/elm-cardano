module Cardano.Transaction exposing
    ( Transaction, new
    , TransactionBody, newBody, ScriptDataHash
    , WitnessSet, newWitnessSet
    , Update
    , ScriptContext, ScriptPurpose(..)
    , Certificate(..), PoolId, GenesisHash, GenesisDelegateHash, VrfKeyHash, RewardSource(..), RewardTarget(..), MoveInstantaneousReward
    , Relay(..), IpV4, IpV6, PoolParams, PoolMetadata, PoolMetadataHash
    , VKeyWitness, BootstrapWitness, Ed25519PublicKey, Ed25519Signature, BootstrapWitnessChainCode, BootstrapWitnessAttributes
    , FeeParameters, RefScriptFeeParameters, defaultTxFeeParams, computeFees, allInputs
    , deserialize, serialize, encodeToCbor
    , decodeWitnessSet
    )

{-| Types and functions related to on-chain transactions.

@docs Transaction, new

@docs TransactionBody, newBody, ScriptDataHash

@docs WitnessSet, newWitnessSet

@docs Update

@docs ScriptContext, ScriptPurpose

@docs Certificate, PoolId, GenesisHash, GenesisDelegateHash, VrfKeyHash, RewardSource, RewardTarget, MoveInstantaneousReward

@docs Relay, IpV4, IpV6, PoolParams, PoolMetadata, PoolMetadataHash

@docs VKeyWitness, BootstrapWitness, Ed25519PublicKey, Ed25519Signature, BootstrapWitnessChainCode, BootstrapWitnessAttributes

@docs FeeParameters, RefScriptFeeParameters, defaultTxFeeParams, computeFees, allInputs

@docs deserialize, serialize, encodeToCbor

@docs decodeWitnessSet

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Credential, CredentialHash, NetworkId(..), StakeAddress, decodeCredential)
import Cardano.AuxiliaryData as AuxiliaryData exposing (AuxiliaryData)
import Cardano.Data as Data exposing (Data)
import Cardano.Gov as Gov exposing (ActionId, Anchor, Drep, ExUnitPrices, ProposalProcedure, ProtocolParamUpdate, RationalNumber, UnitInterval, Voter, VotingProcedure)
import Cardano.MultiAsset as MultiAsset exposing (MultiAsset, PolicyId)
import Cardano.Redeemer as Redeemer exposing (Redeemer)
import Cardano.Script as Script exposing (NativeScript, ScriptCbor)
import Cardano.Utxo as Utxo exposing (Output, OutputReference, encodeOutput, encodeOutputReference)
import Cbor.Decode as D
import Cbor.Decode.Extra as D
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Integer exposing (Integer)
import Natural exposing (Natural)
import RationalNat exposing (RationalNat)


{-| A Cardano transaction.
-}
type alias Transaction =
    { body : TransactionBody -- 0
    , witnessSet : WitnessSet -- 1
    , isValid : Bool -- 2 -- after alonzo
    , auxiliaryData : Maybe AuxiliaryData -- 3
    }


{-| Helper for empty [Transaction] initialization.
-}
new : Transaction
new =
    { body = newBody
    , witnessSet = newWitnessSet
    , isValid = True
    , auxiliaryData = Nothing
    }


{-| A Cardano transaction body.
-}
type alias TransactionBody =
    { inputs : List OutputReference -- 0
    , outputs : List Output -- 1
    , fee : Maybe Natural -- 2 TODO: remove the Maybe
    , ttl : Maybe Natural -- 3 a slot number
    , certificates : List Certificate -- 4
    , withdrawals : List ( StakeAddress, Natural ) -- 5
    , update : Maybe Update -- 6
    , auxiliaryDataHash : Maybe (Bytes AuxiliaryData.Hash) -- 7
    , validityIntervalStart : Maybe Int -- 8
    , mint : MultiAsset Integer -- 9
    , scriptDataHash : Maybe (Bytes ScriptDataHash) -- 11
    , collateral : List OutputReference -- 13
    , requiredSigners : List (Bytes CredentialHash) -- 14
    , networkId : Maybe NetworkId -- 15
    , collateralReturn : Maybe Output -- 16
    , totalCollateral : Maybe Int -- 17
    , referenceInputs : List OutputReference -- 18

    -- New in Conway
    , votingProcedures : List ( Voter, List ( ActionId, VotingProcedure ) ) -- 19 Voting procedures
    , proposalProcedures : List ProposalProcedure -- 20 Proposal procedures
    , currentTreasuryValue : Maybe Natural -- 21 Current treasury value
    , treasuryDonation : Maybe Natural -- 22 Donation
    }


{-| Phantom type for script data hashes.
This is a 32-bytes Blake2b-256 hash.
-}
type ScriptDataHash
    = ScriptDataHash Never


{-| Helper for empty transaction body initialization.
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


{-| A Cardano transaction witness set.

[Pallas alonzo implementation][pallas]

[pallas]: https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L763

-}
type alias WitnessSet =
    { vkeywitness : Maybe (List VKeyWitness) -- 0
    , nativeScripts : Maybe (List NativeScript) -- 1
    , bootstrapWitness : Maybe (List BootstrapWitness) -- 2
    , plutusV1Script : Maybe (List (Bytes ScriptCbor)) -- 3
    , plutusData : Maybe (List Data) -- 4
    , redeemer : Maybe (List Redeemer) -- 5
    , plutusV2Script : Maybe (List (Bytes ScriptCbor)) -- 6
    , plutusV3Script : Maybe (List (Bytes ScriptCbor)) -- 7
    }


{-| Helper for empty witness set initialization.
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


{-| Payload to update the protocol parameters at a specific epoch
-}
type alias Update =
    { proposedProtocolParameterUpdates : BytesMap GenesisHash ProtocolParamUpdate
    , epoch : Natural
    }


{-| VKey witness
-}
type alias VKeyWitness =
    { vkey : Bytes Ed25519PublicKey -- 0
    , signature : Bytes Ed25519Signature -- 1
    }


{-| Bootstrap witness
-}
type alias BootstrapWitness =
    { publicKey : Bytes Ed25519PublicKey -- 0
    , signature : Bytes Ed25519Signature -- 1
    , chainCode : Bytes BootstrapWitnessChainCode -- 2
    , attributes : Bytes BootstrapWitnessAttributes -- 3
    }


{-| Phantom type for ED25519 public keys, of length 32 bytes.
-}
type Ed25519PublicKey
    = Ed25519PublicKey Never


{-| Phantom type for ED25519 signatures, of length 64 bytes.
-}
type Ed25519Signature
    = Ed25519Signature Never


{-| Phantom type for [BootstrapWitness] chain code.
It has a length of 32 bytes.
-}
type BootstrapWitnessChainCode
    = BootstrapWitnessChainCode Never


{-| Phantom type for [BootstrapWitness] attributes.
Bytes of this type can be of any length.
-}
type BootstrapWitnessAttributes
    = BootstrapWitnessAttributes Never



-- Scripts #####################################################################


{-| A context given to a script by the Cardano ledger when being executed.

The context contains information about the entire transaction that contains the script.
The transaction may also contain other scripts.
To distinguish between multiple scripts, the ScriptContext contains a "purpose" identifying the current resource triggering this execution.

-}
type alias ScriptContext =
    { transaction : Transaction
    , purpose : ScriptPurpose
    }


{-| Characterizes the kind of script being executed and the associated resource.
-}
type ScriptPurpose
    = SPMint { policyId : Bytes PolicyId }
    | SPSpend OutputReference
    | SPWithdrawFrom Credential
    | SPPublish Certificate



-- Certificate #################################################################


{-| An on-chain certificate attesting of some operation.
Publishing certificates triggers different kind of rules.
Most of the time, they require signatures from specific keys.
-}
type Certificate
    = StakeRegistration { delegator : Credential } -- 0 (will be deprecated after Conway)
    | StakeDeregistration { delegator : Credential } -- 1 (will be deprecated after Conway)
    | StakeDelegation { delegator : Credential, poolId : Bytes PoolId } -- 2
    | PoolRegistration PoolParams -- 3
    | PoolRetirement { poolId : Bytes PoolId, epoch : Natural } -- 4
    | GenesisKeyDelegation
        -- 5 (deprecated in Conway)
        { genesisHash : Bytes GenesisHash
        , genesisDelegateHash : Bytes GenesisDelegateHash
        , vrfKeyHash : Bytes VrfKeyHash
        }
    | MoveInstantaneousRewardsCert MoveInstantaneousReward -- 6 (deprecated in Conway)
      -- New Conway era certificates: https://sancho.network/tools-resources/faq/
    | RegCert { delegator : Credential, deposit : Natural } -- 7 Registers stake credentials
    | UnregCert { delegator : Credential, refund : Natural } -- 8 Unregisters stake credentials
    | VoteDelegCert { delegator : Credential, drep : Drep } -- 9 Delegates votes
    | StakeVoteDelegCert { delegator : Credential, poolId : Bytes PoolId, drep : Drep } -- 10 Delegates to a stake pool and a DRep from the same certificate
    | StakeRegDelegCert { delegator : Credential, poolId : Bytes PoolId, deposit : Natural } -- 11 Registers stake credentials and delegates to a stake pool
    | VoteRegDelegCert { delegator : Credential, drep : Drep, deposit : Natural } -- 12 Registers stake credentials and delegates to a DRep
    | StakeVoteRegDelegCert { delegator : Credential, poolId : Bytes PoolId, drep : Drep, deposit : Natural } -- 13 Registers stake credentials, delegates to a pool, and to a DRep
    | AuthCommitteeHotCert { commiteeColdCredential : Credential, comiteeHotCredential : Credential } -- 14 Authorizes the constitutional committee hot credential
    | ResignCommitteeColdCert { commiteeColdCredential : Credential, anchor : Maybe Anchor } -- 15 Resigns the constitutional committee cold credential
    | RegDrepCert { drepCredential : Credential, deposit : Natural, anchor : Maybe Anchor } -- 16 Registers DRep's credentials
    | UnregDrepCert { drepCredential : Credential, refund : Natural } -- 17 Unregisters (retires) DRep's credentials
    | UpdateDrepCert { drepCredential : Credential, anchor : Maybe Anchor } -- 18 Updates DRep's metadata anchor


{-| Phantom type for pool ID.
This is a 28-bytes Blake2b-224 hash.

-- TODO: Move Pool stuff into its own module

-}
type PoolId
    = PoolId Never


{-| Phantom type for Genesis hash.
This is a 28-bytes Blake2b-224 hash.
-}
type GenesisHash
    = GenesisHash Never


{-| Phantom type for Genesis delegate hash.
This is a 28-bytes Blake2b-224 hash.
-}
type GenesisDelegateHash
    = GenesisDelegateHash Never


{-| Phantom type for VRF key hash.
This is a 32-bytes Blake2b-256 hash.
-}
type VrfKeyHash
    = VrfKeyHash Never


{-| Parameters for stake pool registration.
-}
type alias PoolParams =
    { operator : Bytes PoolId
    , vrfKeyHash : Bytes VrfKeyHash
    , pledge : Natural
    , cost : Natural
    , margin : UnitInterval
    , rewardAccount : StakeAddress
    , poolOwners : List (Bytes CredentialHash)
    , relays : List Relay
    , poolMetadata : Maybe PoolMetadata
    }


{-| A pool's relay information.
-}
type Relay
    = SingleHostAddr { port_ : Maybe Int, ipv4 : Maybe (Bytes IpV4), ipv6 : Maybe (Bytes IpV6) }
    | SingleHostName { port_ : Maybe Int, dnsName : String }
    | MultiHostName { dnsName : String }


{-| Phantom type for 4-bytes IPV4 addresses.
-}
type IpV4
    = IpV4 Never


{-| Phantom type for 16-bytes IPV6 addresses.
-}
type IpV6
    = IpV6 Never


{-| A pool's metadata hash.
-}
type alias PoolMetadata =
    { url : String -- tstr .size (0..64)
    , poolMetadataHash : Bytes PoolMetadataHash
    }


{-| Phantom type for 32-bytes pool metadata hash.
This is a Blacke2b-256 hash.
-}
type PoolMetadataHash
    = PoolMetadataHash Never


{-| Payload for [MoveInstantaneousRewardsCert].
-}
type alias MoveInstantaneousReward =
    { source : RewardSource
    , target : RewardTarget
    }


{-| The source of rewards.
-}
type RewardSource
    = Reserves -- 0
    | Treasury -- 1


{-| Reward target for a certificate's [MoveInstantaneousReward].

If `StakeCredentials`, funds are moved to stake credentials,
otherwise the funds are given to the other accounting pot.

-}
type RewardTarget
    = StakeCredentials (List ( Credential, Natural ))
    | OtherAccountingPot Natural


{-| Parameters required to compute transaction fees.
-}
type alias FeeParameters =
    { baseFee : Int
    , feePerByte : Int
    , scriptExUnitPrice : ExUnitPrices
    , refScriptFeeParams : RefScriptFeeParameters
    }


{-| Default values for fee parameters.
-}
defaultTxFeeParams : FeeParameters
defaultTxFeeParams =
    { baseFee = 155381
    , feePerByte = 44
    , scriptExUnitPrice =
        { memPrice = { numerator = 577, denominator = 10000 }
        , stepPrice = { numerator = 721, denominator = 10000000 }
        }
    , refScriptFeeParams =
        { minFeeRefScriptCostPerByte = 15
        , multiplier = { numerator = 12, denominator = 10 }
        , sizeIncrement = 25600
        }
    }


{-| Parameters for the costs of referencing scripts.

Full explanation of the formula here:
<https://github.com/IntersectMBO/cardano-ledger/blob/master/docs/adr/2024-08-14_009-refscripts-fee-change.md>

-}
type alias RefScriptFeeParameters =
    { minFeeRefScriptCostPerByte : Int -- lovelace/bytes until reaching the second size level
    , multiplier : RationalNumber -- exponential cost increase for each size level
    , sizeIncrement : Int -- level size (in bytes) for each exponential fee price change
    }


{-| Re-compute fees for a transaction (does not read `body.fee`).
-}
computeFees : FeeParameters -> { refScriptBytes : Int } -> Transaction -> { txSizeFee : Natural, scriptExecFee : Natural, refScriptSizeFee : Natural }
computeFees { baseFee, feePerByte, scriptExUnitPrice, refScriptFeeParams } { refScriptBytes } tx =
    let
        txSize =
            Bytes.width (serialize tx)

        ( totalSteps, totalMem ) =
            tx.witnessSet.redeemer
                |> Maybe.withDefault []
                |> List.foldl
                    (\r ( steps, mem ) ->
                        ( Natural.add steps <| Natural.fromSafeInt r.exUnits.steps
                        , Natural.add mem <| Natural.fromSafeInt r.exUnits.mem
                        )
                    )
                    ( Natural.zero, Natural.zero )

        totalStepsCost =
            Natural.mul totalSteps (Natural.fromSafeInt scriptExUnitPrice.stepPrice.numerator)
                |> Natural.divBy (Natural.fromSafeInt scriptExUnitPrice.stepPrice.denominator)
                |> Maybe.withDefault Natural.zero

        totalMemCost =
            Natural.mul totalMem (Natural.fromSafeInt scriptExUnitPrice.memPrice.numerator)
                |> Natural.divBy (Natural.fromSafeInt scriptExUnitPrice.memPrice.denominator)
                |> Maybe.withDefault Natural.zero
    in
    { txSizeFee = Natural.fromSafeInt (baseFee + feePerByte * txSize)
    , scriptExecFee = Natural.add totalStepsCost totalMemCost
    , refScriptSizeFee = refScriptFee refScriptFeeParams refScriptBytes
    }


{-| Helper function to compute the fees associated with reference script size.

Full explanation of the formula here:
<https://github.com/IntersectMBO/cardano-ledger/blob/master/docs/adr/2024-08-14_009-refscripts-fee-change.md>

```haskell
tierRefScriptFee = go 0 minFeeRefScriptCostPerByte
  where
    go acc curTierPrice n
      | n < sizeIncrement =
          floor (acc + (n % 1) * curTierPrice)
      | otherwise =
          let acc' = acc + curTierPrice * (sizeIncrement % 1)
           in go acc' (multiplier * curTierPrice) (n - sizeIncrement)
    sizeIncrement = 25600
    multiplier = 1.2
    minFeeRefScriptCostPerByte = 15
```

-}
refScriptFee : RefScriptFeeParameters -> Int -> Natural
refScriptFee ({ minFeeRefScriptCostPerByte } as p) refScriptBytes =
    let
        baseTierPricePerByte =
            RationalNat.fromSafeInt minFeeRefScriptCostPerByte
    in
    refScriptFeeHelper p { bytesLeft = refScriptBytes, tierPricePerByte = baseTierPricePerByte } RationalNat.zero
        |> RationalNat.floor
        |> Maybe.withDefault Natural.zero


refScriptFeeHelper : RefScriptFeeParameters -> { bytesLeft : Int, tierPricePerByte : RationalNat } -> RationalNat -> RationalNat
refScriptFeeHelper p { bytesLeft, tierPricePerByte } costAccum =
    if bytesLeft <= p.sizeIncrement then
        RationalNat.mul tierPricePerByte (RationalNat.fromSafeInt bytesLeft)
            |> RationalNat.add costAccum

    else
        let
            newCostAccum =
                RationalNat.mul tierPricePerByte (RationalNat.fromSafeInt p.sizeIncrement)
                    |> RationalNat.add costAccum

            multip =
                RationalNat (Natural.fromSafeInt p.multiplier.numerator) (Natural.fromSafeInt p.multiplier.denominator)
        in
        refScriptFeeHelper p
            { bytesLeft = bytesLeft - p.sizeIncrement
            , tierPricePerByte = RationalNat.mul multip tierPricePerByte
            }
            newCostAccum


{-| Extract all inputs that are used in the transaction,
from inputs, collateral and reference inputs.
-}
allInputs : Transaction -> Utxo.RefDict ()
allInputs tx =
    List.concat
        [ tx.body.inputs
        , tx.body.collateral
        , tx.body.referenceInputs
        ]
        |> List.map (\ref -> ( ref, () ))
        |> Utxo.refDictFromList



-- https://github.com/input-output-hk/cardano-ledger/blob/a792fbff8156773e712ef875d82c2c6d4358a417/eras/babbage/test-suite/cddl-files/babbage.cddl#L13


{-| Serialize a [Transaction] into cbor bytes
-}
serialize : Transaction -> Bytes Transaction
serialize =
    encodeToCbor >> E.encode >> Bytes.fromBytes


{-| Deserialize a transaction's cbor bytes into a [Transaction]
-}
deserialize : Bytes a -> Maybe Transaction
deserialize bytes =
    bytes
        |> Bytes.toBytes
        |> D.decode (D.oneOf [ decodeTransaction, D.failWith "Transaction decoder failed" ])


{-| Encode a Tx to CBOR
-}
encodeToCbor : Transaction -> E.Encoder
encodeToCbor =
    E.tuple <|
        E.elems
            >> E.elem encodeTransactionBody .body
            >> E.elem encodeWitnessSet .witnessSet
            >> E.elem E.bool .isValid
            >> E.elem (E.maybe AuxiliaryData.toCbor) .auxiliaryData


{-| -}
encodeTransactionBody : TransactionBody -> E.Encoder
encodeTransactionBody =
    E.record E.int <|
        E.fields
            >> E.field 0 encodeInputs .inputs
            >> E.field 1 encodeOutputs .outputs
            >> E.optionalField 2 EE.natural .fee
            >> E.optionalField 3 EE.natural .ttl
            >> EE.nonEmptyField 4 List.isEmpty encodeCertificates .certificates
            >> EE.nonEmptyField 5 List.isEmpty (EE.associativeList Address.stakeAddressToCbor EE.natural) .withdrawals
            >> E.optionalField 6 encodeUpdate .update
            >> E.optionalField 7 Bytes.toCbor .auxiliaryDataHash
            >> E.optionalField 8 E.int .validityIntervalStart
            >> EE.nonEmptyField 9 MultiAsset.isEmpty MultiAsset.mintToCbor .mint
            >> E.optionalField 11 Bytes.toCbor .scriptDataHash
            >> EE.nonEmptyField 13 List.isEmpty encodeInputs .collateral
            >> EE.nonEmptyField 14 List.isEmpty encodeRequiredSigners .requiredSigners
            >> E.optionalField 15 Address.encodeNetworkId .networkId
            >> E.optionalField 16 encodeOutput .collateralReturn
            >> E.optionalField 17 E.int .totalCollateral
            >> EE.nonEmptyField 18 List.isEmpty encodeInputs .referenceInputs
            >> EE.nonEmptyField 19 List.isEmpty encodeVotingProcedures .votingProcedures
            >> EE.nonEmptyField 20 List.isEmpty (E.list encodeProposalProcedure) .proposalProcedures
            >> E.optionalField 21 EE.natural .currentTreasuryValue
            >> E.optionalField 22 EE.natural .treasuryDonation


encodeVotingProcedures : List ( Voter, List ( ActionId, VotingProcedure ) ) -> E.Encoder
encodeVotingProcedures =
    EE.associativeList
        Gov.encodeVoter
        (EE.associativeList Gov.encodeActionId Gov.encodeVotingProcedure)


encodeProposalProcedure : ProposalProcedure -> E.Encoder
encodeProposalProcedure =
    E.tuple
        (E.elems
            >> E.elem EE.natural .deposit
            >> E.elem Address.stakeAddressToCbor .rewardAccount
            >> E.elem Gov.encodeAction .govAction
            >> E.elem Gov.encodeAnchor .anchor
        )


{-| -}
encodeWitnessSet : WitnessSet -> E.Encoder
encodeWitnessSet =
    E.record E.int <|
        E.fields
            >> E.optionalField 0 encodeVKeyWitnesses .vkeywitness
            >> E.optionalField 1 (E.list Script.encodeNativeScript) .nativeScripts
            >> E.optionalField 2 encodeBootstrapWitnesses .bootstrapWitness
            >> E.optionalField 3 (E.list Bytes.toCbor) .plutusV1Script
            >> E.optionalField 4 (Data.encodeList Data.toCbor) .plutusData
            >> E.optionalField 5 encodeRedeemersAsMap .redeemer
            >> E.optionalField 6 (E.list Bytes.toCbor) .plutusV2Script
            >> E.optionalField 7 (E.list Bytes.toCbor) .plutusV3Script


{-| -}
encodeVKeyWitnesses : List VKeyWitness -> E.Encoder
encodeVKeyWitnesses v =
    E.list encodeVKeyWitness v


{-| -}
encodeVKeyWitness : VKeyWitness -> E.Encoder
encodeVKeyWitness =
    E.tuple <|
        E.elems
            >> E.elem Bytes.toCbor .vkey
            >> E.elem Bytes.toCbor .signature


{-| -}
encodeBootstrapWitnesses : List BootstrapWitness -> E.Encoder
encodeBootstrapWitnesses b =
    E.list encodeBootstrapWitness b


{-| -}
encodeBootstrapWitness : BootstrapWitness -> E.Encoder
encodeBootstrapWitness =
    E.tuple <|
        E.elems
            >> E.elem Bytes.toCbor .publicKey
            >> E.elem Bytes.toCbor .signature


{-| -}
encodeRedeemersAsMap : List Redeemer -> E.Encoder
encodeRedeemersAsMap redeemers =
    List.map (\r -> ( ( r.tag, r.index ), ( r.data, r.exUnits ) )) redeemers
        |> EE.associativeList
            (E.tuple <|
                E.elems
                    >> E.elem Redeemer.encodeTag Tuple.first
                    >> E.elem E.int Tuple.second
            )
            (E.tuple <|
                E.elems
                    >> E.elem Data.toCbor Tuple.first
                    >> E.elem Redeemer.encodeExUnits Tuple.second
            )


{-| -}
encodeInputs : List OutputReference -> E.Encoder
encodeInputs inputs =
    E.list encodeOutputReference inputs


{-| -}
encodeOutputs : List Output -> E.Encoder
encodeOutputs outputs =
    E.list encodeOutput outputs


{-| -}
encodeCertificates : List Certificate -> E.Encoder
encodeCertificates =
    E.list encodeCertificate


{-| -}
encodeCertificate : Certificate -> E.Encoder
encodeCertificate certificate =
    E.list identity <|
        case certificate of
            StakeRegistration { delegator } ->
                [ E.int 0
                , Address.credentialToCbor delegator
                ]

            StakeDeregistration { delegator } ->
                [ E.int 1
                , Address.credentialToCbor delegator
                ]

            StakeDelegation { delegator, poolId } ->
                [ E.int 2
                , Address.credentialToCbor delegator
                , Bytes.toCbor poolId
                ]

            PoolRegistration poolParams ->
                [ E.int 3
                , Bytes.toCbor poolParams.operator
                , Bytes.toCbor poolParams.vrfKeyHash
                , EE.natural poolParams.pledge
                , EE.natural poolParams.cost
                , Gov.encodeRationalNumber poolParams.margin
                , Address.stakeAddressToCbor poolParams.rewardAccount
                , E.list Bytes.toCbor poolParams.poolOwners
                , E.list encodeRelay poolParams.relays
                , E.maybe encodePoolMetadata poolParams.poolMetadata
                ]

            PoolRetirement { poolId, epoch } ->
                [ E.int 4
                , Bytes.toCbor poolId
                , EE.natural epoch
                ]

            GenesisKeyDelegation { genesisHash, genesisDelegateHash, vrfKeyHash } ->
                [ E.int 5
                , Bytes.toCbor genesisHash
                , Bytes.toCbor genesisDelegateHash
                , Bytes.toCbor vrfKeyHash
                ]

            MoveInstantaneousRewardsCert moveInstantaneousReward ->
                [ E.int 6
                , encodeMoveInstantaneousReward moveInstantaneousReward
                ]

            -- 7 Registers stake credentials
            RegCert { delegator, deposit } ->
                [ E.int 7
                , Address.credentialToCbor delegator
                , EE.natural deposit
                ]

            -- 8 Unregisters stake credentials
            UnregCert { delegator, refund } ->
                [ E.int 8
                , Address.credentialToCbor delegator
                , EE.natural refund
                ]

            -- 9 Delegates votes
            VoteDelegCert { delegator, drep } ->
                [ E.int 9
                , Address.credentialToCbor delegator
                , Gov.encodeDrep drep
                ]

            -- 10 Delegates to a stake pool and a DRep from the same certificate
            StakeVoteDelegCert { delegator, poolId, drep } ->
                [ E.int 10
                , Address.credentialToCbor delegator
                , Bytes.toCbor poolId
                , Gov.encodeDrep drep
                ]

            -- 11 Registers stake credentials and delegates to a stake pool
            StakeRegDelegCert { delegator, poolId, deposit } ->
                [ E.int 11
                , Address.credentialToCbor delegator
                , Bytes.toCbor poolId
                , EE.natural deposit
                ]

            -- 12 Registers stake credentials and delegates to a DRep
            VoteRegDelegCert { delegator, drep, deposit } ->
                [ E.int 12
                , Address.credentialToCbor delegator
                , Gov.encodeDrep drep
                , EE.natural deposit
                ]

            -- 13 Registers stake credentials, delegates to a pool, and to a DRep
            StakeVoteRegDelegCert { delegator, poolId, drep, deposit } ->
                [ E.int 13
                , Address.credentialToCbor delegator
                , Bytes.toCbor poolId
                , Gov.encodeDrep drep
                , EE.natural deposit
                ]

            -- 14 Authorizes the constitutional committee hot credential
            AuthCommitteeHotCert { commiteeColdCredential, comiteeHotCredential } ->
                [ E.int 14
                , Address.credentialToCbor commiteeColdCredential
                , Address.credentialToCbor comiteeHotCredential
                ]

            -- 15 Resigns the constitutional committee cold credential
            ResignCommitteeColdCert { commiteeColdCredential, anchor } ->
                [ E.int 15
                , Address.credentialToCbor commiteeColdCredential
                , E.maybe Gov.encodeAnchor anchor
                ]

            -- 16 Registers DRep's credentials
            RegDrepCert { drepCredential, deposit, anchor } ->
                [ E.int 16
                , Address.credentialToCbor drepCredential
                , EE.natural deposit
                , E.maybe Gov.encodeAnchor anchor
                ]

            -- 17 Unregisters (retires) DRep's credentials
            UnregDrepCert { drepCredential, refund } ->
                [ E.int 17
                , Address.credentialToCbor drepCredential
                , EE.natural refund
                ]

            -- 18 Updates DRep's metadata anchor
            UpdateDrepCert { drepCredential, anchor } ->
                [ E.int 18
                , Address.credentialToCbor drepCredential
                , E.maybe Gov.encodeAnchor anchor
                ]


encodeRelay : Relay -> E.Encoder
encodeRelay relay =
    E.list identity <|
        case relay of
            SingleHostAddr { port_, ipv4, ipv6 } ->
                [ E.int 0
                , E.maybe E.int port_
                , E.maybe Bytes.toCbor ipv4
                , E.maybe Bytes.toCbor ipv6
                ]

            SingleHostName { port_, dnsName } ->
                [ E.int 1
                , E.maybe E.int port_
                , E.string dnsName
                ]

            MultiHostName { dnsName } ->
                [ E.int 2
                , E.string dnsName
                ]


encodePoolMetadata : PoolMetadata -> E.Encoder
encodePoolMetadata =
    E.tuple <|
        E.elems
            >> E.elem E.string .url
            >> E.elem Bytes.toCbor .poolMetadataHash


encodeMoveInstantaneousReward : MoveInstantaneousReward -> E.Encoder
encodeMoveInstantaneousReward =
    E.tuple <|
        E.elems
            >> E.elem encodeRewardSource .source
            >> E.elem encodeRewardTarget .target


encodeRewardSource : RewardSource -> E.Encoder
encodeRewardSource source =
    E.int <|
        case source of
            Reserves ->
                0

            Treasury ->
                1


encodeRewardTarget : RewardTarget -> E.Encoder
encodeRewardTarget target =
    case target of
        StakeCredentials distribution ->
            EE.associativeList Address.credentialToCbor EE.natural distribution

        OtherAccountingPot n ->
            EE.natural n


{-| -}
encodeRequiredSigners : List (Bytes CredentialHash) -> E.Encoder
encodeRequiredSigners =
    E.list Bytes.toCbor


{-| -}
encodeUpdate : Update -> E.Encoder
encodeUpdate =
    E.tuple <|
        E.elems
            >> E.elem encodeProposedProtocolParameterUpdates .proposedProtocolParameterUpdates
            >> E.elem EE.natural .epoch


{-| -}
encodeProposedProtocolParameterUpdates : BytesMap GenesisHash ProtocolParamUpdate -> E.Encoder
encodeProposedProtocolParameterUpdates =
    Bytes.Map.toCbor Gov.encodeProtocolParamUpdate


{-| -}
decodeTransaction : D.Decoder Transaction
decodeTransaction =
    let
        preAlonzo =
            D.tuple (\body witness auxiliary -> { body = body, witnessSet = witness, isValid = True, auxiliaryData = auxiliary }) <|
                D.elems
                    >> D.elem (D.oneOf [ decodeBody, D.failWith "Failed to decode body" ])
                    >> D.elem (D.oneOf [ decodeWitnessSet, D.failWith "Failed to decode witness" ])
                    >> D.elem (D.oneOf [ D.maybe AuxiliaryData.fromCbor, D.failWith "Failed to decode auxiliary" ])

        postAlonzo =
            D.tuple Transaction <|
                D.elems
                    >> D.elem (D.oneOf [ decodeBody, D.failWith "Failed to decode body" ])
                    >> D.elem (D.oneOf [ decodeWitnessSet, D.failWith "Failed to decode witness" ])
                    >> D.elem D.bool
                    >> D.elem (D.oneOf [ D.maybe AuxiliaryData.fromCbor, D.failWith "Failed to decode auxiliary" ])
    in
    D.oneOf [ postAlonzo, preAlonzo ]



-- Decode body


decodeBody : D.Decoder TransactionBody
decodeBody =
    let
        buildTxBody inputs outputs fee ttl certificates withdrawals update auxiliaryDataHash validityIntervalStart mint scriptDataHash collateral requiredSigners networkId collateralReturn totalCollateral referenceInputs votingProcedures proposalProcedures currentTreasuryValue treasuryDonation =
            { inputs = inputs
            , outputs = outputs
            , fee = Just fee
            , ttl = ttl
            , certificates = certificates |> Maybe.withDefault []
            , withdrawals = withdrawals |> Maybe.withDefault []
            , update = update
            , auxiliaryDataHash = auxiliaryDataHash
            , validityIntervalStart = validityIntervalStart
            , mint = mint |> Maybe.withDefault MultiAsset.empty
            , scriptDataHash = scriptDataHash
            , collateral = collateral |> Maybe.withDefault []
            , requiredSigners = requiredSigners |> Maybe.withDefault []
            , networkId = networkId
            , collateralReturn = collateralReturn
            , totalCollateral = totalCollateral
            , referenceInputs = referenceInputs |> Maybe.withDefault []
            , votingProcedures = votingProcedures |> Maybe.withDefault []
            , proposalProcedures = proposalProcedures |> Maybe.withDefault []
            , currentTreasuryValue = currentTreasuryValue
            , treasuryDonation = treasuryDonation
            }
    in
    D.record D.int buildTxBody <|
        D.fields
            -- inputs
            >> D.field 0
                (D.oneOf
                    [ D.set Utxo.decodeOutputReference
                    , D.failWith "Failed to decode inputs (0)"
                    ]
                )
            -- outputs
            >> D.field 1
                (D.oneOf
                    [ D.list Utxo.decodeOutput
                    , D.failWith "Failed to decode outputs (1)"
                    ]
                )
            -- fee
            >> D.field 2
                (D.oneOf [ D.natural, D.failWith "Failed to decode fee (2)" ])
            -- ttl
            >> D.optionalField 3
                (D.oneOf [ D.natural, D.failWith "Failed to decode TTL (3)" ])
            -- certificates
            >> D.optionalField 4
                (D.oneOf
                    [ D.set decodeCertificate
                    , D.failWith "Failed to decode certificates (4)"
                    ]
                )
            -- withdrawals
            >> D.optionalField 5
                (D.oneOf [ decodeWithdrawals, D.failWith "Failed to decode withdrawals (5)" ])
            -- update
            >> D.optionalField 6
                (D.oneOf [ decodeUpdate, D.failWith "Failed to decode protocol update (6)" ])
            -- auxiliary data hash
            >> D.optionalField 7
                (D.oneOf
                    [ D.map Bytes.fromBytes D.bytes
                    , D.failWith "Failed to decode auxiliary data hash (7)"
                    ]
                )
            -- validity interval start
            >> D.optionalField 8
                (D.oneOf [ D.int, D.failWith "Failed to decode validity interval start (8)" ])
            -- mint
            >> D.optionalField 9
                (D.oneOf [ MultiAsset.mintFromCbor, D.failWith "Failed to decode mint (9)" ])
            -- script data hash
            >> D.optionalField 11
                (D.oneOf
                    [ D.map Bytes.fromBytes D.bytes
                    , D.failWith "Failed to decode script data hash (11)"
                    ]
                )
            -- collateral
            >> D.optionalField 13
                (D.oneOf
                    [ D.set Utxo.decodeOutputReference
                    , D.failWith "Failed to decode collateral (13)"
                    ]
                )
            -- required signers
            >> D.optionalField 14
                (D.oneOf
                    [ D.set (D.map Bytes.fromBytes D.bytes)
                    , D.failWith "Failed to decode required signers (14)"
                    ]
                )
            -- network ID
            >> D.optionalField 15
                (D.oneOf [ decodeNetworkId, D.failWith "Failed to decode network id (15)" ])
            -- collateral return
            >> D.optionalField 16
                (D.oneOf [ Utxo.decodeOutput, D.failWith "Failed to decode collateral return (16)" ])
            -- total collateral
            >> D.optionalField 17
                (D.oneOf [ D.int, D.failWith "Failed to decode total collateral (17)" ])
            -- reference inputs
            >> D.optionalField 18
                (D.oneOf
                    [ D.set Utxo.decodeOutputReference
                    , D.failWith "Failed to decode reference inputs (18)"
                    ]
                )
            -- votingProcedures : List ( Voter, List ( ActionId, VotingProcedure ) ) -- 19 Voting procedures
            >> D.optionalField 19
                (D.oneOf
                    [ D.associativeList
                        Gov.voterFromCbor
                        (D.associativeList Gov.actionIdFromCbor Gov.votingProcedureFromCbor)
                    , D.failWith "Failed to decode voting procedures (19)"
                    ]
                )
            -- proposalProcedures : List ProposalProcedure -- 20 Proposal procedures
            >> D.optionalField 20
                (D.oneOf
                    [ D.set Gov.proposalProcedureFromCbor
                    , D.failWith "Failed to decode proposal procedures (20)"
                    ]
                )
            -- currentTreasuryValue : Maybe Natural -- 21 Current treasury value
            >> D.optionalField 21
                (D.oneOf [ D.natural, D.failWith "Failed to decode current treasury value (21)" ])
            -- treasuryDonation : Maybe Natural -- 22 Donation
            >> D.optionalField 22
                (D.oneOf [ D.natural, D.failWith "Failed to decode treasury donation (22)" ])


decodeBodyFold : D.Decoder TransactionBody
decodeBodyFold =
    D.fold D.int
        (\k ->
            case k of
                -- inputs
                0 ->
                    D.oneOf
                        [ D.list Utxo.decodeOutputReference |> D.map setInputs
                        , D.failWith "Failed to decode inputs (0)"
                        ]

                -- outputs
                1 ->
                    D.oneOf
                        [ D.list Utxo.decodeOutput |> D.map setOutputs
                        , D.failWith "Failed to decode outputs (1)"
                        ]

                -- fee
                2 ->
                    D.oneOf [ D.natural |> D.map setFee, D.failWith "Failed to decode fee (2)" ]

                -- ttl
                3 ->
                    D.oneOf [ D.natural |> D.map setTtl, D.failWith "Failed to decode TTL (3)" ]

                -- certificates
                4 ->
                    D.oneOf
                        [ D.set decodeCertificate |> D.map setCertificates
                        , D.failWith "Failed to decode certificate (4)"
                        ]

                -- withdrawals
                5 ->
                    D.oneOf [ decodeWithdrawals |> D.map setWithdrawals, D.failWith "Failed to decode withdrawals (5)" ]

                -- update
                6 ->
                    D.oneOf [ decodeUpdate |> D.map setUpdate, D.failWith "Failed to decode protocol update (6)" ]

                -- auxiliary data hash
                7 ->
                    D.oneOf
                        [ D.map Bytes.fromBytes D.bytes |> D.map setAuxiliaryDataHash
                        , D.failWith "Failed to decode auxiliary data hash (7)"
                        ]

                -- validity interval start
                8 ->
                    D.oneOf [ D.int |> D.map setValidityIntervalStart, D.failWith "Failed to decode validity interval start (8)" ]

                -- mint
                9 ->
                    D.oneOf [ MultiAsset.mintFromCbor |> D.map setMint, D.failWith "Failed to decode mint (9)" ]

                -- (DEPRECATED) expansion rate
                10 ->
                    D.succeed identity

                -- script data hash
                11 ->
                    D.oneOf
                        [ D.map Bytes.fromBytes D.bytes |> D.map setScriptDataHash
                        , D.failWith "Failed to decode script data hash (11)"
                        ]

                -- (DEPRECATED) decentralization constant
                12 ->
                    D.succeed identity

                -- collateral
                13 ->
                    D.oneOf
                        [ D.list Utxo.decodeOutputReference |> D.map setCollateral
                        , D.failWith "Failed to decode collateral (13)"
                        ]

                -- required signers
                14 ->
                    D.oneOf
                        [ D.list (D.map Bytes.fromBytes D.bytes) |> D.map setRequiredSigners
                        , D.failWith "Failed to decode required signers (14)"
                        ]

                -- network ID
                15 ->
                    D.oneOf [ decodeNetworkId |> D.map setNetworkId, D.failWith "Failed to decode network id (15)" ]

                -- collateral return
                16 ->
                    D.oneOf [ Utxo.decodeOutput |> D.map setCollateralReturn, D.failWith "Failed to decode collateral return (16)" ]

                -- total collateral
                17 ->
                    D.oneOf [ D.int |> D.map setTotalCollateral, D.failWith "Failed to decode total collateral (17)" ]

                -- reference inputs
                18 ->
                    D.oneOf
                        [ D.list Utxo.decodeOutputReference |> D.map setReferenceInputs
                        , D.failWith "Failed to decode reference inputs (18)"
                        ]

                _ ->
                    D.failWith ("Unknown tx body tag: " ++ String.fromInt k)
        )
        newBody


decodeCertificate : D.Decoder Certificate
decodeCertificate =
    D.length
        |> D.andThen
            (\length ->
                D.int |> D.andThen (\id -> decodeCertificateHelper length id)
            )


decodeCertificateHelper : Int -> Int -> D.Decoder Certificate
decodeCertificateHelper length id =
    case ( length, id ) of
        -- stake_registration = (0, stake_credential)
        ( 2, 0 ) ->
            D.map (\cred -> StakeRegistration { delegator = cred }) decodeCredential

        -- stake_deregistration = (1, stake_credential)
        ( 2, 1 ) ->
            D.map (\cred -> StakeDeregistration { delegator = cred }) decodeCredential

        -- stake_delegation = (2, stake_credential, pool_keyhash)
        ( 3, 2 ) ->
            D.map2
                (\cred poolId -> StakeDelegation { delegator = cred, poolId = poolId })
                decodeCredential
                (D.map Bytes.fromBytes D.bytes)

        -- pool_registration = (3, pool_params)
        -- pool_params is of size 9
        ( 10, 3 ) ->
            D.map PoolRegistration <| D.oneOf [ decodePoolParams, D.failWith "Failed to decode pool params" ]

        -- pool_retirement = (4, pool_keyhash, epoch)
        ( 3, 4 ) ->
            D.map2 (\poolId epoch -> PoolRetirement { poolId = poolId, epoch = epoch })
                (D.map Bytes.fromBytes D.bytes)
                D.natural

        -- genesis_key_delegation = (5, genesishash, genesis_delegate_hash, vrf_keyhash)
        ( 4, 5 ) ->
            D.map3
                (\genHash genDelHash vrfKeyHash ->
                    GenesisKeyDelegation
                        { genesisHash = genHash
                        , genesisDelegateHash = genDelHash
                        , vrfKeyHash = vrfKeyHash
                        }
                )
                (D.map Bytes.fromBytes D.bytes)
                (D.map Bytes.fromBytes D.bytes)
                (D.map Bytes.fromBytes D.bytes)

        -- move_instantaneous_rewards_cert = (6, move_instantaneous_reward)
        ( 2, 6 ) ->
            D.map MoveInstantaneousRewardsCert decodeMoveInstantaneousRewards

        -- reg_cert = (7, credential, coin)
        ( 3, 7 ) ->
            D.map2
                (\cred deposit -> RegCert { delegator = cred, deposit = deposit })
                decodeCredential
                D.natural

        -- unreg_cert = (8, credential, coin)
        ( 3, 8 ) ->
            D.map2
                (\cred refund -> UnregCert { delegator = cred, refund = refund })
                decodeCredential
                D.natural

        -- vote_deleg_cert = (9, credential, drep)
        ( 3, 9 ) ->
            D.map2
                (\cred drep -> VoteDelegCert { delegator = cred, drep = drep })
                (D.oneOf [ decodeCredential, D.failWith "decodeCredential failed" ])
                (D.oneOf [ Gov.decodeDrep, D.failWith "decodeDrep failed" ])

        -- stake_vote_deleg_cert = (10, credential, pool_keyhash, drep)
        ( 4, 10 ) ->
            D.map3
                (\cred poolId drep -> StakeVoteDelegCert { delegator = cred, poolId = poolId, drep = drep })
                decodeCredential
                (D.map Bytes.fromBytes D.bytes)
                Gov.decodeDrep

        -- stake_reg_deleg_cert = (11, credential, pool_keyhash, coin)
        ( 4, 11 ) ->
            D.map3
                (\cred poolId deposit -> StakeRegDelegCert { delegator = cred, poolId = poolId, deposit = deposit })
                decodeCredential
                (D.map Bytes.fromBytes D.bytes)
                D.natural

        -- vote_reg_deleg_cert = (12, credential, drep, coin)
        ( 4, 12 ) ->
            D.map3
                (\cred drep deposit -> VoteRegDelegCert { delegator = cred, drep = drep, deposit = deposit })
                decodeCredential
                Gov.decodeDrep
                D.natural

        -- stake_vote_reg_deleg_cert = (13, credential, pool_keyhash, drep, coin)
        ( 5, 13 ) ->
            D.map4
                (\cred poolId drep deposit -> StakeVoteRegDelegCert { delegator = cred, poolId = poolId, drep = drep, deposit = deposit })
                decodeCredential
                (D.map Bytes.fromBytes D.bytes)
                Gov.decodeDrep
                D.natural

        -- auth_committee_hot_cert = (14, committee_cold_credential, committee_hot_credential)
        ( 3, 14 ) ->
            D.map2
                (\cold hot -> AuthCommitteeHotCert { commiteeColdCredential = cold, comiteeHotCredential = hot })
                decodeCredential
                decodeCredential

        -- resign_committee_cold_cert = (15, committee_cold_credential, anchor / nil)
        ( 3, 15 ) ->
            D.map2
                (\cold anchor -> ResignCommitteeColdCert { commiteeColdCredential = cold, anchor = anchor })
                decodeCredential
                (D.maybe Gov.decodeAnchor)

        -- reg_drep_cert = (16, drep_credential, coin, anchor / nil)
        ( 4, 16 ) ->
            D.map3
                (\cred deposit anchor -> RegDrepCert { drepCredential = cred, deposit = deposit, anchor = anchor })
                decodeCredential
                D.natural
                (D.maybe Gov.decodeAnchor)

        -- unreg_drep_cert = (17, drep_credential, coin)
        ( 3, 17 ) ->
            D.map2
                (\cred refund -> UnregDrepCert { drepCredential = cred, refund = refund })
                decodeCredential
                D.natural

        -- update_drep_cert = (18, drep_credential, anchor / nil)
        ( 3, 18 ) ->
            D.map2
                (\cred anchor -> UpdateDrepCert { drepCredential = cred, anchor = anchor })
                decodeCredential
                (D.maybe Gov.decodeAnchor)

        _ ->
            D.failWith <|
                "Unknown length and id for certificate ("
                    ++ String.fromInt length
                    ++ ", "
                    ++ String.fromInt id
                    ++ ")"


decodePoolParams : D.Decoder PoolParams
decodePoolParams =
    D.succeed PoolParams
        |> D.keep (D.oneOf [ D.map Bytes.fromBytes D.bytes, D.failWith "Failed to decode operator" ])
        |> D.keep (D.oneOf [ D.map Bytes.fromBytes D.bytes, D.failWith "Failed to decode vrfkeyhash" ])
        |> D.keep (D.oneOf [ D.natural, D.failWith "Failed to decode pledge" ])
        |> D.keep D.natural
        |> D.keep (D.oneOf [ Gov.decodeRational, D.failWith "Failed to decode rational" ])
        |> D.keep (D.oneOf [ Address.decodeReward, D.failWith "Failed to decode reward" ])
        |> D.keep (D.set (D.map Bytes.fromBytes D.bytes))
        |> D.keep (D.list <| D.oneOf [ decodeRelay, D.failWith "Failed to decode Relay" ])
        |> D.keep (D.oneOf [ D.maybe decodePoolMetadata, D.failWith "Failed to decode pool metadata" ])


decodeRelay : D.Decoder Relay
decodeRelay =
    D.length
        |> D.andThen (\length -> D.int |> D.andThen (decodeRelayHelper length))


decodeRelayHelper : Int -> Int -> D.Decoder Relay
decodeRelayHelper length id =
    case ( length, id ) of
        -- single_host_addr = ( 0, port / null, ipv4 / null, ipv6 / null )
        ( 4, 0 ) ->
            D.map3 (\port_ ipv4 ipv6 -> SingleHostAddr { port_ = port_, ipv4 = ipv4, ipv6 = ipv6 })
                (D.maybe D.int)
                (D.maybe <| D.map Bytes.fromBytes D.bytes)
                (D.maybe <| D.map Bytes.fromBytes D.bytes)

        -- single_host_name = ( 1, port / null, dns_name )  -- An A or AAAA DNS record
        ( 3, 1 ) ->
            D.map2 (\port_ dns -> SingleHostName { port_ = port_, dnsName = dns })
                (D.maybe D.int)
                D.string

        -- multi_host_name = ( 2, dns_name )  -- A SRV DNS record
        ( 2, 2 ) ->
            D.map (\dns -> MultiHostName { dnsName = dns })
                D.string

        _ ->
            D.failWith <|
                "Unknown length and id for relay ("
                    ++ String.fromInt length
                    ++ ", "
                    ++ String.fromInt id
                    ++ ")"


decodePoolMetadata : D.Decoder PoolMetadata
decodePoolMetadata =
    D.tuple PoolMetadata <|
        D.elems
            >> D.elem D.string
            >> D.elem (D.map Bytes.fromBytes D.bytes)


decodeMoveInstantaneousRewards : D.Decoder MoveInstantaneousReward
decodeMoveInstantaneousRewards =
    D.tuple (\source targets -> { source = source, target = StakeCredentials targets }) <|
        D.elems
            >> D.elem decodeRewardSource
            >> D.elem (D.associativeList decodeCredential D.natural)


decodeRewardSource : D.Decoder RewardSource
decodeRewardSource =
    D.int
        |> D.andThen
            (\source ->
                case source of
                    0 ->
                        D.succeed Reserves

                    1 ->
                        D.succeed Treasury

                    _ ->
                        D.failWith "Unknown reward source"
            )


decodeWithdrawals : D.Decoder (List ( StakeAddress, Natural ))
decodeWithdrawals =
    D.associativeList Address.decodeReward D.natural


decodeUpdate : D.Decoder Update
decodeUpdate =
    D.tuple (\updates epoch -> { proposedProtocolParameterUpdates = Bytes.Map.fromList updates, epoch = epoch }) <|
        D.elems
            >> D.elem (D.associativeList (D.map Bytes.fromBytes D.bytes) Gov.decodeProtocolParamUpdate)
            >> D.elem D.natural



-- Decode witness


{-| Decode a [WitnessSet] from CBOR.
-}
decodeWitnessSet : D.Decoder WitnessSet
decodeWitnessSet =
    -- TODO: Make it fail for an unknown field. Maybe use D.fold instead.
    D.record D.int WitnessSet <|
        D.fields
            -- vkeywitness
            >> D.optionalField 0 (D.oneOf [ D.set decodeVKeyWitness, D.failWith "Failed to decode KVeyWitness list" ])
            -- multisig_script
            >> D.optionalField 1 (D.oneOf [ D.set Script.decodeNativeScript, D.failWith "Failed to decode NativeScript list" ])
            -- bootstrap_witness
            >> D.optionalField 2 (D.oneOf [ D.set decodeBootstrapWitness, D.failWith "Failed to decode bootstrap witness" ])
            -- plutus_v1_script
            >> D.optionalField 3 (D.oneOf [ D.set (D.map Bytes.fromBytes D.bytes), D.failWith "Failed to decode plutus v1 script" ])
            -- plutus_data
            >> D.optionalField 4 (D.oneOf [ D.set Data.fromCbor, D.failWith "Failed to decode plutus data" ])
            -- redeemer: decode as either array or maps in conway
            >> D.optionalField 5
                (D.oneOf
                    [ D.list Redeemer.fromCborArray
                    , D.associativeList
                        -- [tag, index]
                        (D.tuple Tuple.pair <|
                            D.elems
                                >> D.elem Redeemer.tagFromCbor
                                >> D.elem D.int
                        )
                        -- [data, exUnits]
                        (D.tuple Tuple.pair <|
                            D.elems
                                >> D.elem Data.fromCbor
                                >> D.elem Redeemer.exUnitsFromCbor
                        )
                        |> D.map (List.map (\( ( tag, index ), ( data, exUnits ) ) -> Redeemer tag index data exUnits))
                    , D.failWith "Failed to decode redeemer"
                    ]
                )
            -- plutus_v2_script
            >> D.optionalField 6 (D.oneOf [ D.set (D.map Bytes.fromBytes D.bytes), D.failWith "Failed to decode plutus v2 script" ])
            -- plutus_v3_script
            >> D.optionalField 7 (D.oneOf [ D.set (D.map Bytes.fromBytes D.bytes), D.failWith "Failed to decode plutus v3 script" ])


decodeVKeyWitness : D.Decoder VKeyWitness
decodeVKeyWitness =
    D.tuple
        (\vkey sig ->
            { vkey = Bytes.fromBytes vkey
            , signature = Bytes.fromBytes sig
            }
        )
    <|
        D.elems
            >> D.elem D.bytes
            >> D.elem D.bytes


decodeBootstrapWitness : D.Decoder BootstrapWitness
decodeBootstrapWitness =
    D.tuple
        (\pubkey sig chainCode attr ->
            { publicKey = Bytes.fromBytes pubkey
            , signature = Bytes.fromBytes sig
            , chainCode = Bytes.fromBytes chainCode
            , attributes = Bytes.fromBytes attr
            }
        )
    <|
        D.elems
            >> D.elem D.bytes
            >> D.elem D.bytes
            >> D.elem D.bytes
            >> D.elem D.bytes


decodeNetworkId : D.Decoder NetworkId
decodeNetworkId =
    D.int
        |> D.andThen
            (\id ->
                case id of
                    0 ->
                        D.succeed Testnet

                    1 ->
                        D.succeed Mainnet

                    _ ->
                        D.failWith ("Uknown network id: " ++ String.fromInt id)
            )



-- Helper definitions


setInputs : List OutputReference -> TransactionBody -> TransactionBody
setInputs inputs body =
    { body | inputs = inputs }


setOutputs : List Output -> TransactionBody -> TransactionBody
setOutputs outputs body =
    { body | outputs = outputs }


setFee : Natural -> TransactionBody -> TransactionBody
setFee fee body =
    { body | fee = Just fee }


setTtl : Natural -> TransactionBody -> TransactionBody
setTtl ttl body =
    { body | ttl = Just ttl }


setCertificates : List Certificate -> TransactionBody -> TransactionBody
setCertificates certificates body =
    { body | certificates = certificates }


setWithdrawals : List ( StakeAddress, Natural ) -> TransactionBody -> TransactionBody
setWithdrawals withdrawals body =
    { body | withdrawals = withdrawals }


setUpdate : Update -> TransactionBody -> TransactionBody
setUpdate update body =
    { body | update = Just update }


setAuxiliaryDataHash : Bytes AuxiliaryData.Hash -> TransactionBody -> TransactionBody
setAuxiliaryDataHash hash body =
    { body | auxiliaryDataHash = Just hash }


setValidityIntervalStart : Int -> TransactionBody -> TransactionBody
setValidityIntervalStart start body =
    { body | validityIntervalStart = Just start }


setMint : MultiAsset Integer -> TransactionBody -> TransactionBody
setMint mint body =
    { body | mint = mint }


setScriptDataHash : Bytes ScriptDataHash -> TransactionBody -> TransactionBody
setScriptDataHash hash body =
    { body | scriptDataHash = Just hash }


setCollateral : List OutputReference -> TransactionBody -> TransactionBody
setCollateral collateral body =
    { body | collateral = collateral }


setRequiredSigners : List (Bytes CredentialHash) -> TransactionBody -> TransactionBody
setRequiredSigners signers body =
    { body | requiredSigners = signers }


setNetworkId : NetworkId -> TransactionBody -> TransactionBody
setNetworkId networkId body =
    { body | networkId = Just networkId }


setCollateralReturn : Output -> TransactionBody -> TransactionBody
setCollateralReturn collateralReturn body =
    { body | collateralReturn = Just collateralReturn }


setTotalCollateral : Int -> TransactionBody -> TransactionBody
setTotalCollateral totalCollateral body =
    { body | totalCollateral = Just totalCollateral }


setReferenceInputs : List OutputReference -> TransactionBody -> TransactionBody
setReferenceInputs refInputs body =
    { body | referenceInputs = refInputs }
