module ElmCardano.Transaction exposing
    ( Transaction
    , TransactionBody, WitnessSet
    , NetworkId(..)
    , AuxiliaryData(..), Metadata, Metadatum(..)
    , Update, ProtocolParamUpdate, ProtocolVersion
    , ScriptContext, ScriptPurpose(..)
    , Certificate(..), StakeCredential(..), RewardSource(..), RewardTarget(..), MoveInstantaneousReward
    , Relay(..), PoolParams, PoolMetadata
    , CostModels, ExUnitPrices
    , RationalNumber, UnitInterval, PositiveInterval
    , VKeyWitness, BootstrapWitness
    , deserialize, serialize
    )

{-| Types and functions related to on-chain transactions.

@docs Transaction

@docs TransactionBody, WitnessSet

@docs NetworkId

@docs AuxiliaryData, Metadata, Metadatum

@docs Update, ProtocolParamUpdate, ProtocolVersion

@docs ScriptContext, ScriptPurpose

@docs Certificate, StakeCredential, RewardSource, RewardTarget, MoveInstantaneousReward

@docs Relay, PoolParams, PoolMetadata

@docs CostModels, ExUnitPrices

@docs RationalNumber, UnitInterval, PositiveInterval

@docs VKeyWitness, BootstrapWitness

@docs deserialize, serialize

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import BytesMap exposing (BytesMap)
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Encode.Extra as E
import Cbor.Tag as Tag
import Debug exposing (todo)
import Dict exposing (Dict)
import ElmCardano.Data as Data exposing (Data)
import ElmCardano.Hash as Hash exposing (Blake2b_224, Blake2b_256, Hash)
import ElmCardano.MultiAsset as MultiAsset exposing (MultiAsset)
import ElmCardano.Redeemer as Redeemer exposing (ExUnits, Redeemer)
import ElmCardano.Script as Script exposing (NativeScript, PlutusScript)
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


{-| metadata used in [AuxiliaryData].
-}
type alias Metadata =
    Dict Int Metadatum


{-| [Transaction] auxiliary data.
-}
type AuxiliaryData
    = Shelley Metadata
    | ShelleyMa { transactionMetadata : Metadata, auxiliaryScripts : List NativeScript }
    | PostAlonzo
        { metadata : Maybe (Dict Int Metadatum) -- 0
        , nativeScripts : Maybe (List NativeScript) -- 1
        , plutusV1Scripts : Maybe (List PlutusScript) -- 2
        , plutusV2Scripts : Maybe (List PlutusScript) -- 2
        }


{-| Payload to update the protocol parameters at a specific epoch
-}
type alias Update =
    { proposedProtocolParameterUpdates : BytesMap (Hash Blake2b_224) ProtocolParamUpdate
    , epoch : Int
    }


{-| Adjustable parameters that power key aspects of the network.
-}
type alias ProtocolParamUpdate =
    { minFeeA : Maybe Int -- 0
    , minFeeB : Maybe Int -- 1
    , maxBlockBodySize : Maybe Int -- 2
    , maxTransactionSize : Maybe Int -- 3
    , maxBlockHeaderSize : Maybe Int -- 4
    , keyDeposit : Maybe Int -- 5
    , poolDeposit : Maybe Int -- 6
    , maximumEpoch : Maybe Int -- 7
    , desiredNumberOfStakePools : Maybe Int -- 8
    , poolPledgeInfluence : Maybe RationalNumber -- 9
    , expansionRate : Maybe UnitInterval -- 10
    , treasuryGrowthRate : Maybe UnitInterval -- 11
    , protocolVersion : Maybe ProtocolVersion -- 14
    , minPoolCost : Maybe Int -- 16
    , adaPerUtxoByte : Maybe Int -- 17
    , costModelsForScriptLanguages : Maybe CostModels -- 18
    , executionCosts : Maybe ExUnitPrices -- 19
    , maxTxExUnits : Maybe ExUnits -- 20
    , maxBlockExUnits : Maybe ExUnits -- 21
    , maxValueSize : Maybe Int -- 22
    , collateralPercentage : Maybe Int -- 23
    , maxCollateralInputs : Maybe Int -- 24
    }


{-| -}
type alias CostModels =
    { plutusV1 : Maybe (List Int) -- 0
    , plutusV2 : Maybe (List Int) -- 1
    }


{-| -}
type alias ExUnitPrices =
    { memPrice : PositiveInterval -- 0
    , stepPrice : PositiveInterval -- 1
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
    | Map (Dict Metadatum Metadatum)


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
    = StakeRegistration { delegator : StakeCredential }
    | StakeDeregistration { delegator : StakeCredential }
    | StakeDelegation { delegator : StakeCredential, delegatee : Hash Blake2b_224 }
    | PoolRegistration PoolParams
    | PoolRetirement { poolId : Hash Blake2b_224, epoch : Int }
    | GenesisKeyDelegation
        { genesisHash : Hash Blake2b_224
        , genesisDelegateHash : Hash Blake2b_224
        , vrfKeyHash : Hash Blake2b_256
        }
    | MoveInstantaneousRewardsCert MoveInstantaneousReward


{-| A credential used in certificates.
-}
type StakeCredential
    = AddrKeyHash (Hash Blake2b_224)
    | ScriptHash (Hash Blake2b_224)


{-| Parameters for stake pool registration.
-}
type alias PoolParams =
    { operator : Hash Blake2b_224
    , vrfKeyHash : Hash Blake2b_256
    , pledge : Int
    , cost : Int
    , margin : UnitInterval
    , rewardAccount : Bytes
    , poolOwners : List (Hash Blake2b_224)
    , relays : List Relay
    , poolMetadata : Maybe PoolMetadata
    }


{-| A pool's relay information.
-}
type Relay
    = SingleHostAddr { port_ : Maybe Int, ipv4 : Maybe Bytes, ipv6 : Maybe Bytes }
    | SingleHostName { port_ : Maybe Int, dnsName : String }
    | MultiHostName { dnsName : String }


{-| A pool's metadata hash.
-}
type alias PoolMetadata =
    { url : String -- tstr .size (0..64)
    , poolMetadataHash : Hash Blake2b_256
    }


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
    = StakeCredentials (Dict StakeCredential Int)
    | OtherAccountingPot Int



-- https://github.com/input-output-hk/cardano-ledger/blob/a792fbff8156773e712ef875d82c2c6d4358a417/eras/babbage/test-suite/cddl-files/babbage.cddl#L13


{-| Serialize a [Transaction] into cbor bytes
-}
serialize : Transaction -> Bytes
serialize =
    encodeTransaction >> E.encode >> Bytes.fromBytes


{-| Deserialize a transaction's cbor bytes into a [Transaction]
-}
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
            >> E.nonEmptyField 9 MultiAsset.isEmpty MultiAsset.toCbor .mint
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
            >> E.optionalField 1 (E.list Script.encodeNativeScript) .nativeScripts
            >> E.optionalField 2 encodeBootstrapWitnesses .bootstrapWitness
            >> E.optionalField 3 (E.list Bytes.toCbor) .plutusV1Script
            >> E.optionalField 4 (E.indefiniteList Data.toCbor) .plutusData
            >> E.optionalField 5 (E.list Redeemer.encode) .redeemer
            >> E.optionalField 6 (E.list Bytes.toCbor) .plutusV2Script


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
encodeAuxiliaryData auxiliaryData =
    let
        encodeMetadata =
            E.dict E.int encodeMetadatum
    in
    case auxiliaryData of
        Shelley metadata ->
            encodeMetadata metadata

        ShelleyMa data ->
            data
                |> E.tuple
                    (E.elems
                        >> E.elem encodeMetadata .transactionMetadata
                        >> E.elem (E.list Script.encodeNativeScript) .auxiliaryScripts
                    )

        PostAlonzo data ->
            data
                |> E.tagged (Tag.Unknown 259)
                    (E.record E.int
                        (E.fields
                            >> E.optionalField 0 encodeMetadata .metadata
                            >> E.optionalField 1 (E.list Script.encodeNativeScript) .nativeScripts
                            >> E.optionalField 2 (E.list Script.encodePlutusScript) .plutusV1Scripts
                            >> E.optionalField 3 (E.list Script.encodePlutusScript) .plutusV2Scripts
                        )
                    )


encodeMetadatum : Metadatum -> E.Encoder
encodeMetadatum metadatum =
    case metadatum of
        Int n ->
            E.int n

        Bytes bytes ->
            Bytes.toCbor bytes

        String str ->
            E.string str

        List metadatums ->
            E.list encodeMetadatum metadatums

        Map metadatums ->
            E.dict encodeMetadatum encodeMetadatum metadatums


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
                , encodeStakeCredential delegator
                ]

            StakeDeregistration { delegator } ->
                [ E.int 1
                , encodeStakeCredential delegator
                ]

            StakeDelegation { delegator, delegatee } ->
                [ E.int 2
                , encodeStakeCredential delegator
                , Hash.encode delegatee
                ]

            PoolRegistration poolParams ->
                [ E.int 3
                , Hash.encode poolParams.operator
                , Hash.encode poolParams.vrfKeyHash
                , E.int poolParams.pledge
                , E.int poolParams.cost
                , encodeRationalNumber poolParams.margin
                , Bytes.toCbor poolParams.rewardAccount
                , E.list Hash.encode poolParams.poolOwners
                , E.list encodeRelay poolParams.relays
                , E.maybe encodePoolMetadata poolParams.poolMetadata
                ]

            PoolRetirement { poolId, epoch } ->
                [ E.int 4
                , Hash.encode poolId
                , E.int epoch
                ]

            GenesisKeyDelegation { genesisHash, genesisDelegateHash, vrfKeyHash } ->
                [ E.int 5
                , Hash.encode genesisHash
                , Hash.encode genesisDelegateHash
                , Hash.encode vrfKeyHash
                ]

            MoveInstantaneousRewardsCert moveInstantaneousReward ->
                [ E.int 6
                , encodeMoveInstantaneousReward moveInstantaneousReward
                ]


encodeStakeCredential : StakeCredential -> E.Encoder
encodeStakeCredential stakeCredential =
    E.list identity <|
        case stakeCredential of
            AddrKeyHash addrKeyHash ->
                [ E.int 0
                , Hash.encode addrKeyHash
                ]

            ScriptHash scriptHash ->
                [ E.int 1
                , Hash.encode scriptHash
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
            >> E.elem Hash.encode .poolMetadataHash


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
            E.dict encodeStakeCredential E.int distribution

        OtherAccountingPot n ->
            E.int n


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
            >> E.optionalField 9 encodeRationalNumber .poolPledgeInfluence
            >> E.optionalField 10 encodeRationalNumber .expansionRate
            >> E.optionalField 11 encodeRationalNumber .treasuryGrowthRate
            >> E.optionalField 14 (\( v, m ) -> E.list E.int [ v, m ]) .protocolVersion
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
            >> E.elem encodeRationalNumber .memPrice
            >> E.elem encodeRationalNumber .stepPrice


encodeCostModels : CostModels -> E.Encoder
encodeCostModels =
    E.record E.int <|
        E.fields
            >> E.optionalField 0 (E.list E.int) .plutusV1
            >> E.optionalField 1 (E.list E.int) .plutusV2


encodeRationalNumber : RationalNumber -> E.Encoder
encodeRationalNumber =
    E.tagged (Tag.Unknown 30) <|
        E.tuple <|
            E.elems
                >> E.elem E.int .numerator
                >> E.elem E.int .denominator


{-| -}
decodeTransaction : D.Decoder Transaction
decodeTransaction =
    -- TODO: decode tx
    D.fail
