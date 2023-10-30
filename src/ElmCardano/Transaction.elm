module ElmCardano.Transaction exposing
    ( Transaction
    , TransactionBody, WitnessSet
    , NetworkId(..)
    , AuxiliaryData
    , Update, ProtocolParamUpdate, ProtocolVersion
    , ScriptContext, ScriptPurpose(..)
    , Certificate(..)
    , CostModels, ExUnitPrices
    , RationalNumber, UnitInterval, PositiveInterval
    , Metadatum(..)
    , VKeyWitness, BootstrapWitness
    , deserialize, serialize
    )

{-| Types and functions related to on-chain transactions.

@docs Transaction

@docs TransactionBody, WitnessSet

@docs NetworkId

@docs AuxiliaryData

@docs Update, ProtocolParamUpdate, ProtocolVersion

@docs ScriptContext, ScriptPurpose

@docs Certificate

@docs CostModels, ExUnitPrices

@docs RationalNumber, UnitInterval, PositiveInterval

@docs Metadatum

@docs VKeyWitness, BootstrapWitness

@docs deserialize, serialize

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import BytesMap exposing (BytesMap)
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Encode.Extra as E
import Debug exposing (todo)
import Dict exposing (Dict)
import ElmCardano.Address exposing (StakeCredential)
import ElmCardano.Data as Data exposing (Data)
import ElmCardano.Hash as Hash exposing (Blake2b_224, Blake2b_256, Hash)
import ElmCardano.MultiAsset as MultiAsset exposing (MultiAsset)
import ElmCardano.Redeemer as Redeemer exposing (ExUnits, Redeemer)
import ElmCardano.Script exposing (NativeScript, PlutusScript)
import ElmCardano.Utxo exposing (Output, OutputReference, encodeOutput, encodeOutputReference)


{-| A Cardano transaction.
-}
type alias Transaction =
    { body : TransactionBody -- 0
    , witnessSet : WitnessSet -- 1
    , isValid : Bool -- 2
    , auxiliaryData : Maybe AuxiliaryData -- 3
    }


{-| A Cardano transaction body.
-}
type alias TransactionBody =
    { inputs : List OutputReference -- 0
    , outputs : List Output -- 1
    , fee : Maybe Int -- 2
    , ttl : Maybe Int -- 3
    , certificates : List Certificate -- 4
    , withdrawals : BytesMap Bytes Int -- 5
    , update : Maybe Update -- 6
    , auxiliaryDataHash : Maybe (Hash Blake2b_256) -- 7
    , validityIntervalStart : Maybe Int -- 8
    , mint : MultiAsset -- 9
    , scriptDataHash : Maybe Bytes -- 11
    , collateral : List OutputReference -- 13
    , requiredSigners : List (Hash Blake2b_224) -- 14
    , networkId : Maybe NetworkId -- 15
    , collateralReturn : Maybe Output -- 16
    , totalCollateral : Maybe Int -- 17
    , referenceInputs : List OutputReference -- 18
    }


{-| The network ID of a transaction.
-}
type NetworkId
    = Testnet -- 0
    | Mainnet -- 1


{-| A Cardano transaction witness set.

[Pallas alonzo implementation][pallas]

[pallas]: https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L763

-}
type alias WitnessSet =
    { vkeywitness : Maybe (List VKeyWitness) -- 0
    , nativeScripts : Maybe (List NativeScript) -- 1
    , bootstrapWitness : Maybe (List BootstrapWitness) -- 2
    , plutusV1Script : Maybe (List Bytes) -- 3
    , plutusData : Maybe (List Data) -- 4
    , redeemer : Maybe (List Redeemer) -- 5
    , plutusV2Script : Maybe (List Bytes) -- 6
    }


{-| -}
type alias AuxiliaryData =
    { metadata : Maybe (Dict Int Metadatum) -- 0
    , nativeScripts : Maybe (List NativeScript) -- 1
    , plutusScripts : Maybe (List PlutusScript) -- 1
    }


{-| -}
type alias Update =
    { proposedProtocolParameterUpdates : BytesMap (Hash Blake2b_224) ProtocolParamUpdate
    , epoch : Int
    }


{-| -}
type alias ProtocolParamUpdate =
    { -- #[n(0)]
      minFeeA : Maybe Int
    , -- #[n(1)]
      minFeeB : Maybe Int
    , -- #[n(2)]
      maxBlockBodySize : Maybe Int
    , -- #[n(3)]
      maxTransactionSize : Maybe Int
    , -- #[n(4)]
      maxBlockHeaderSize : Maybe Int
    , -- #[n(5)]
      keyDeposit : Maybe Int
    , -- #[n(6)]
      poolDeposit : Maybe Int
    , -- #[n(7)]
      maximumEpoch : Maybe Int
    , -- #[n(8)]
      desiredNumberOfStakePools : Maybe Int
    , -- #[n(9)]
      poolPledgeInfluence : Maybe RationalNumber
    , -- #[n(10)]
      expansionRate : Maybe UnitInterval
    , -- #[n(11)]
      treasuryGrowthRate : Maybe UnitInterval
    , -- #[n(14)]
      protocolVersion : Maybe ProtocolVersion
    , -- #[n(16)]
      minPoolCost : Maybe Int
    , -- #[n(17)]
      adaPerUtxoByte : Maybe Int
    , -- #[n(18)]
      costModelsForScriptLanguages : Maybe CostModels
    , -- #[n(19)]
      executionCosts : Maybe ExUnitPrices
    , -- #[n(20)]
      maxTxExUnits : Maybe ExUnits
    , -- #[n(21)]
      maxBlockExUnits : Maybe ExUnits
    , -- #[n(22)]
      maxValueSize : Maybe Int
    , -- #[n(23)]
      collateralPercentage : Maybe Int
    , -- #[n(24)]
      maxCollateralInputs : Maybe Int
    }


{-| -}
type alias CostModels =
    { -- #[n(0)]
      plutusV1 : Maybe (List Int)
    , -- #[n(1)]
      plutusV2 : Maybe (List Int)
    }


{-| -}
type alias ExUnitPrices =
    { -- #[n(0)]
      memPrice : PositiveInterval
    , -- #[n(1)]
      stepPrice : PositiveInterval
    }


{-| -}
type alias ProtocolVersion =
    ( Int, Int )


{-| -}
type alias UnitInterval =
    RationalNumber


{-| -}
type alias PositiveInterval =
    RationalNumber



-- https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L379


{-| -}
type alias RationalNumber =
    { numerator : Int
    , denominator : Int
    }


{-| -}
type Metadatum
    = Int Int
    | Bytes Bytes
    | String String
    | List (List Metadatum)
    | Map (List ( Metadatum, Metadatum ))


{-| -}
type alias VKeyWitness =
    { vkey : Bytes -- 0
    , signature : Bytes
    }



-- TODO: what kinds of hashes are these?


{-| -}
type alias BootstrapWitness =
    { publicKey : Bytes -- 0
    , signature : Bytes -- 1
    , chainCode : Bytes -- 2
    , attributes : Bytes -- 3
    }



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
    = SPMint { policyId : Hash Blake2b_224 }
    | SPSpend OutputReference
    | SPWithdrawFrom StakeCredential
    | SPPublish Certificate



-- Certificate #################################################################


{-| An on-chain certificate attesting of some operation.
Publishing certificates triggers different kind of rules.
Most of the time, they require signatures from specific keys.
-}
type Certificate
    = CredentialRegistration { delegator : StakeCredential }
    | CredentialDeregistration { delegator : StakeCredential }
    | CredentialDelegation { delegator : StakeCredential, delegatee : Hash Blake2b_224 }
    | PoolRegistration { poolId : Hash Blake2b_224, vrf : Hash Blake2b_224 }
    | PoolDeregistration { poolId : Hash Blake2b_224, epoch : Int }
    | Governance
    | TreasuryMovement



-- https://github.com/input-output-hk/cardano-ledger/blob/a792fbff8156773e712ef875d82c2c6d4358a417/eras/babbage/test-suite/cddl-files/babbage.cddl#L13


{-| -}
serialize : Transaction -> Bytes
serialize =
    encodeTransaction >> E.encode >> Bytes.fromBytes


{-| -}
deserialize : Bytes -> Maybe Transaction
deserialize bytes =
    Bytes.toBytes bytes
        |> D.decode decodeTransaction


{-| -}
encodeTransaction : Transaction -> E.Encoder
encodeTransaction =
    E.tuple <|
        E.elems
            >> E.elem encodeTransactionBody .body
            >> E.elem encodeWitnessSet .witnessSet
            >> E.elem E.bool .isValid
            >> E.elem (E.maybe encodeAuxiliaryData) .auxiliaryData


{-| -}
encodeTransactionBody : TransactionBody -> E.Encoder
encodeTransactionBody =
    E.record E.int <|
        E.fields
            >> E.field 0 encodeInputs .inputs
            >> E.field 1 encodeOutputs .outputs
            >> E.optionalField 2 E.int .fee
            >> E.optionalField 3 E.int .ttl
            >> E.nonEmptyField 4 List.isEmpty encodeCertificates .certificates
            >> E.nonEmptyField 5 BytesMap.isEmpty (BytesMap.toCbor E.int) .withdrawals
            >> E.optionalField 6 encodeUpdate .update
            >> E.optionalField 7 Hash.encode .auxiliaryDataHash
            >> E.optionalField 8 E.int .validityIntervalStart
            >> E.nonEmptyField 9 MultiAsset.isEmpty (\_ -> todo "Multiasset.toCbor") .mint
            >> E.optionalField 11 Bytes.toCbor .scriptDataHash
            >> E.nonEmptyField 13 List.isEmpty encodeInputs .collateral
            >> E.nonEmptyField 14 List.isEmpty encodeRequiredSigners .requiredSigners
            >> E.optionalField 15 encodeNetworkId .networkId
            >> E.optionalField 16 encodeOutput .collateralReturn
            >> E.optionalField 17 E.int .totalCollateral
            >> E.nonEmptyField 18 List.isEmpty encodeInputs .referenceInputs


{-| -}
encodeNetworkId : NetworkId -> E.Encoder
encodeNetworkId networkId =
    E.int <|
        case networkId of
            Testnet ->
                0

            Mainnet ->
                1


{-| -}
encodeWitnessSet : WitnessSet -> E.Encoder
encodeWitnessSet =
    E.record E.int <|
        E.fields
            >> E.optionalField 0 encodeVKeyWitnesses .vkeywitness
            >> E.optionalField 1 (\_ -> todo "") .nativeScripts
            >> E.optionalField 2 encodeBootstrapWitnesses .bootstrapWitness
            >> E.optionalField 3 (\scripts -> E.list Bytes.toCbor scripts) .plutusV1Script
            >> E.optionalField 4 (E.indefiniteList Data.toCbor) .plutusData
            >> E.optionalField 5 (\redeemers -> E.list Redeemer.encode redeemers) .redeemer
            >> E.optionalField 6 (\scripts -> E.list Bytes.toCbor scripts) .plutusV2Script


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
encodeAuxiliaryData : AuxiliaryData -> E.Encoder
encodeAuxiliaryData _ =
    todo "encode auxiliary data"


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
encodeCertificate _ =
    todo "encode certificate"


{-| -}
encodeRequiredSigners : List (Hash Blake2b_224) -> E.Encoder
encodeRequiredSigners =
    E.list Hash.encode


{-| -}
encodeUpdate : Update -> E.Encoder
encodeUpdate =
    E.tuple <|
        E.elems
            >> E.elem encodeProposedProtocolParameterUpdates .proposedProtocolParameterUpdates
            >> E.elem E.int .epoch


{-| -}
encodeProposedProtocolParameterUpdates : BytesMap (Hash Blake2b_224) ProtocolParamUpdate -> E.Encoder
encodeProposedProtocolParameterUpdates =
    BytesMap.toCbor encodeProtocolParamUpdate


encodeProtocolParamUpdate : ProtocolParamUpdate -> E.Encoder
encodeProtocolParamUpdate =
    E.record E.int <|
        E.fields
            >> E.optionalField 0 E.int .minFeeA
            >> E.optionalField 1 E.int .minFeeB
            >> E.optionalField 2 E.int .maxBlockBodySize
            >> E.optionalField 3 E.int .maxTransactionSize
            >> E.optionalField 4 E.int .maxBlockHeaderSize
            >> E.optionalField 5 E.int .keyDeposit
            >> E.optionalField 6 E.int .poolDeposit
            >> E.optionalField 7 E.int .maximumEpoch
            >> E.optionalField 8 E.int .desiredNumberOfStakePools
            >> E.optionalField 9 (\_ -> todo "RationalNumber.toCbor") .poolPledgeInfluence
            >> E.optionalField 10 (\_ -> todo "RationalNumber.toCbor") .expansionRate
            >> E.optionalField 11 (\_ -> todo "RationalNumber.toCbor") .treasuryGrowthRate
            >> E.optionalField 14 (\_ -> todo "ProtocolVersion.toCbor") .protocolVersion
            >> E.optionalField 16 E.int .minPoolCost
            >> E.optionalField 17 E.int .adaPerUtxoByte
            >> E.optionalField 18 encodeCostModels .costModelsForScriptLanguages
            >> E.optionalField 19 encodeExUnitPrices .executionCosts
            >> E.optionalField 20 Redeemer.encodeExUnits .maxTxExUnits
            >> E.optionalField 21 Redeemer.encodeExUnits .maxBlockExUnits
            >> E.optionalField 22 E.int .maxValueSize
            >> E.optionalField 23 E.int .collateralPercentage
            >> E.optionalField 24 E.int .maxCollateralInputs


encodeExUnitPrices : ExUnitPrices -> E.Encoder
encodeExUnitPrices =
    E.tuple <|
        E.elems
            >> E.elem (\_ -> todo "RationalNumber.toCbor") .memPrice
            >> E.elem (\_ -> todo "RationalNumber.toCbor") .stepPrice


encodeCostModels : CostModels -> E.Encoder
encodeCostModels =
    E.record E.int <|
        E.fields
            >> E.optionalField 0 (E.list E.int) .plutusV1
            >> E.optionalField 1 (E.list E.int) .plutusV2


{-| -}
decodeTransaction : D.Decoder Transaction
decodeTransaction =
    -- TODO: decode tx
    D.fail
