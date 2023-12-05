module ElmCardano.Transaction exposing
    ( Transaction
    , TransactionBody, AuxiliaryDataHash, ScriptDataHash
    , WitnessSet
    , AuxiliaryData(..), Metadata, Metadatum(..), MetadatumBytes
    , Update, ProtocolParamUpdate, ProtocolVersion
    , ScriptContext, ScriptPurpose(..)
    , Certificate(..), PoolId, GenesisHash, GenesisDelegateHash, VrfKeyHash, RewardSource(..), RewardTarget(..), MoveInstantaneousReward
    , Relay(..), IpV4, IpV6, PoolParams, PoolMetadata, PoolMetadataHash
    , CostModels, ExUnitPrices
    , RationalNumber, UnitInterval, PositiveInterval
    , VKeyWitness, BootstrapWitness, Ed25519PublicKey, Ed25519Signature, BootstrapWitnessChainCode, BootstrapWitnessAttributes
    , deserialize, serialize
    )

{-| Types and functions related to on-chain transactions.

@docs Transaction

@docs TransactionBody, AuxiliaryDataHash, ScriptDataHash

@docs WitnessSet

@docs AuxiliaryData, Metadata, Metadatum, MetadatumBytes

@docs Update, ProtocolParamUpdate, ProtocolVersion

@docs ScriptContext, ScriptPurpose

@docs Certificate, PoolId, GenesisHash, GenesisDelegateHash, VrfKeyHash, RewardSource, RewardTarget, MoveInstantaneousReward

@docs Relay, IpV4, IpV6, PoolParams, PoolMetadata, PoolMetadataHash

@docs CostModels, ExUnitPrices

@docs RationalNumber, UnitInterval, PositiveInterval

@docs VKeyWitness, BootstrapWitness, Ed25519PublicKey, Ed25519Signature, BootstrapWitnessChainCode, BootstrapWitnessAttributes

@docs deserialize, serialize

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map exposing (BytesMap)
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Encode.Extra as E
import Cbor.Tag as Tag
import Dict exposing (Dict)
import ElmCardano.Address as Address exposing (Credential, CredentialHash, NetworkId, StakeAddress)
import ElmCardano.Data as Data exposing (Data)
import ElmCardano.MultiAsset as MultiAsset exposing (MultiAsset, PolicyId)
import ElmCardano.Redeemer as Redeemer exposing (ExUnits, Redeemer)
import ElmCardano.Script as Script exposing (NativeScript, PlutusScript, ScriptCbor)
import ElmCardano.Utxo as Utxo exposing (Output, OutputReference, encodeOutput, encodeOutputReference)


{-| A Cardano transaction.
-}
type alias Transaction =
    { body : TransactionBody -- 0
    , witnessSet : WitnessSet -- 1
    , isValid : Bool -- 2 -- after alonzo
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
    , withdrawals : List ( StakeAddress, Int ) -- 5
    , update : Maybe Update -- 6
    , auxiliaryDataHash : Maybe (Bytes AuxiliaryDataHash) -- 7
    , validityIntervalStart : Maybe Int -- 8
    , mint : MultiAsset -- 9
    , scriptDataHash : Maybe (Bytes ScriptDataHash) -- 11
    , collateral : List OutputReference -- 13
    , requiredSigners : List (Bytes CredentialHash) -- 14
    , networkId : Maybe NetworkId -- 15
    , collateralReturn : Maybe Output -- 16
    , totalCollateral : Maybe Int -- 17
    , referenceInputs : List OutputReference -- 18
    }


{-| Phantom type for auxiliary data hashes.
This is a 32-bytes Blake2b-256 hash.
-}
type AuxiliaryDataHash
    = AuxiliaryDataHash Never


{-| Phantom type for script data hashes.
This is a 32-bytes Blake2b-256 hash.
-}
type ScriptDataHash
    = ScriptDataHash Never


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
    { proposedProtocolParameterUpdates : BytesMap GenesisHash ProtocolParamUpdate
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
    | Bytes (Bytes MetadatumBytes)
    | String String
    | List (List Metadatum)
    | Map (List ( Metadatum, Metadatum ))


{-| Phantom type for the Bytes variant of Metadatum.
It is supposed to have a length <= 64 bytes according to the CDDL.
-}
type MetadatumBytes
    = MetadatumBytes Never


{-| -}
type alias VKeyWitness =
    { vkey : Bytes Ed25519PublicKey -- 0
    , signature : Bytes Ed25519Signature -- 1
    }



-- TODO: what kinds of hashes are these?


{-| -}
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
    = StakeRegistration { delegator : Credential }
    | StakeDeregistration { delegator : Credential }
    | StakeDelegation { delegator : Credential, poolId : Bytes PoolId }
    | PoolRegistration PoolParams
    | PoolRetirement { poolId : Bytes PoolId, epoch : Int }
    | GenesisKeyDelegation
        { genesisHash : Bytes GenesisHash
        , genesisDelegateHash : Bytes GenesisDelegateHash
        , vrfKeyHash : Bytes VrfKeyHash
        }
    | MoveInstantaneousRewardsCert MoveInstantaneousReward


{-| Phantom type for pool ID.
This is a 28-bytes Blake2b-224 hash.
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
    , pledge : Int
    , cost : Int
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
    = StakeCredentials (List ( Credential, Int ))
    | OtherAccountingPot Int



-- https://github.com/input-output-hk/cardano-ledger/blob/a792fbff8156773e712ef875d82c2c6d4358a417/eras/babbage/test-suite/cddl-files/babbage.cddl#L13


{-| Serialize a [Transaction] into cbor bytes
-}
serialize : Transaction -> Bytes Transaction
serialize =
    encodeTransaction >> E.encode >> Bytes.fromBytes


{-| Deserialize a transaction's cbor bytes into a [Transaction]
-}
deserialize : Bytes a -> Maybe Transaction
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
            >> E.nonEmptyField 5 List.isEmpty (E.ledgerAssociativeList Address.stakeAddressToCbor E.int) .withdrawals
            >> E.optionalField 6 encodeUpdate .update
            >> E.optionalField 7 Bytes.toCbor .auxiliaryDataHash
            >> E.optionalField 8 E.int .validityIntervalStart
            >> E.nonEmptyField 9 MultiAsset.isEmpty MultiAsset.toCbor .mint
            >> E.optionalField 11 Bytes.toCbor .scriptDataHash
            >> E.nonEmptyField 13 List.isEmpty encodeInputs .collateral
            >> E.nonEmptyField 14 List.isEmpty encodeRequiredSigners .requiredSigners
            >> E.optionalField 15 Address.encodeNetworkId .networkId
            >> E.optionalField 16 encodeOutput .collateralReturn
            >> E.optionalField 17 E.int .totalCollateral
            >> E.nonEmptyField 18 List.isEmpty encodeInputs .referenceInputs


{-| -}
encodeWitnessSet : WitnessSet -> E.Encoder
encodeWitnessSet =
    E.record E.int <|
        E.fields
            >> E.optionalField 0 encodeVKeyWitnesses .vkeywitness
            >> E.optionalField 1 (E.ledgerList Script.encodeNativeScript) .nativeScripts
            >> E.optionalField 2 encodeBootstrapWitnesses .bootstrapWitness
            >> E.optionalField 3 (E.ledgerList Bytes.toCbor) .plutusV1Script
            >> E.optionalField 4 (E.indefiniteList Data.toCbor) .plutusData
            >> E.optionalField 5 (E.ledgerList Redeemer.encode) .redeemer
            >> E.optionalField 6 (E.ledgerList Bytes.toCbor) .plutusV2Script


{-| -}
encodeVKeyWitnesses : List VKeyWitness -> E.Encoder
encodeVKeyWitnesses v =
    E.ledgerList encodeVKeyWitness v


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
    E.ledgerList encodeBootstrapWitness b


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
            E.ledgerDict E.int encodeMetadatum
    in
    case auxiliaryData of
        Shelley metadata ->
            encodeMetadata metadata

        ShelleyMa data ->
            data
                |> E.tuple
                    (E.elems
                        >> E.elem encodeMetadata .transactionMetadata
                        >> E.elem (E.ledgerList Script.encodeNativeScript) .auxiliaryScripts
                    )

        PostAlonzo data ->
            data
                |> E.tagged (Tag.Unknown 259)
                    (E.record E.int
                        (E.fields
                            >> E.optionalField 0 encodeMetadata .metadata
                            >> E.optionalField 1 (E.ledgerList Script.encodeNativeScript) .nativeScripts
                            >> E.optionalField 2 (E.ledgerList Script.encodePlutusScript) .plutusV1Scripts
                            >> E.optionalField 3 (E.ledgerList Script.encodePlutusScript) .plutusV2Scripts
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
            E.ledgerList encodeMetadatum metadatums

        Map metadatums ->
            E.ledgerAssociativeList encodeMetadatum encodeMetadatum metadatums


{-| -}
encodeInputs : List OutputReference -> E.Encoder
encodeInputs inputs =
    E.ledgerList encodeOutputReference inputs


{-| -}
encodeOutputs : List Output -> E.Encoder
encodeOutputs outputs =
    E.ledgerList encodeOutput outputs


{-| -}
encodeCertificates : List Certificate -> E.Encoder
encodeCertificates =
    E.ledgerList encodeCertificate


{-| -}
encodeCertificate : Certificate -> E.Encoder
encodeCertificate certificate =
    E.ledgerList identity <|
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
                , E.int poolParams.pledge
                , E.int poolParams.cost
                , encodeRationalNumber poolParams.margin
                , Address.stakeAddressToCbor poolParams.rewardAccount
                , E.ledgerList Bytes.toCbor poolParams.poolOwners
                , E.ledgerList encodeRelay poolParams.relays
                , E.maybe encodePoolMetadata poolParams.poolMetadata
                ]

            PoolRetirement { poolId, epoch } ->
                [ E.int 4
                , Bytes.toCbor poolId
                , E.int epoch
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


encodeRelay : Relay -> E.Encoder
encodeRelay relay =
    E.ledgerList identity <|
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
            E.ledgerAssociativeList Address.credentialToCbor E.int distribution

        OtherAccountingPot n ->
            E.int n


{-| -}
encodeRequiredSigners : List (Bytes CredentialHash) -> E.Encoder
encodeRequiredSigners =
    E.ledgerList Bytes.toCbor


{-| -}
encodeUpdate : Update -> E.Encoder
encodeUpdate =
    E.tuple <|
        E.elems
            >> E.elem encodeProposedProtocolParameterUpdates .proposedProtocolParameterUpdates
            >> E.elem E.int .epoch


{-| -}
encodeProposedProtocolParameterUpdates : BytesMap GenesisHash ProtocolParamUpdate -> E.Encoder
encodeProposedProtocolParameterUpdates =
    Bytes.Map.toCbor encodeProtocolParamUpdate


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
            >> E.optionalField 14 (\( v, m ) -> E.ledgerList E.int [ v, m ]) .protocolVersion
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
            >> E.optionalField 0 (E.ledgerList E.int) .plutusV1
            >> E.optionalField 1 (E.ledgerList E.int) .plutusV2


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
    D.tuple (\body witness auxiliary -> { body = body, witnessSet = witness, isValid = True, auxiliaryData = auxiliary }) <|
        D.elems
            >> D.elem decodeBody
            >> D.elem decodeWitness
            >> D.elem (D.maybe decodeAuxiliary)


decodeBody : D.Decoder TransactionBody
decodeBody =
    let
        bodyBuilder inputs outputs fee ttl certificates withdrawals update auxiliaryDataHash =
            { newBody
                | inputs = inputs
                , outputs = outputs
                , fee = Just fee
                , ttl = Just ttl
                , certificates = Maybe.withDefault [] certificates
                , withdrawals = Maybe.withDefault [] withdrawals
                , update = update
                , auxiliaryDataHash = auxiliaryDataHash
            }
    in
    D.record D.int bodyBuilder <|
        D.fields
            -- inputs
            >> D.field 0 (D.list Utxo.decodeOutputReference)
            -- outputs
            >> D.field 1 (D.list Utxo.decodeOutput)
            -- fee
            >> D.field 2 D.int
            -- ttl
            >> D.field 3 D.int
            -- certificates
            >> D.optionalField 4 (D.list decodeCertificate)
            -- withdrawals
            >> D.optionalField 5 decodeWithdrawals
            -- update
            >> D.optionalField 6 decodeUpdate
            -- metadata hash
            >> D.optionalField 7 (D.map Bytes.fromBytes D.bytes)


decodeCertificate : D.Decoder Certificate
decodeCertificate =
    failWithMessage "decodeCertificate failed to decode"


decodeWithdrawals : D.Decoder (List ( StakeAddress, Int ))
decodeWithdrawals =
    failWithMessage "decodeWithdrawals failed to decode"


decodeUpdate : D.Decoder Update
decodeUpdate =
    failWithMessage "decodeUpdate failed to decode"


decodeWitness : D.Decoder WitnessSet
decodeWitness =
    let
        witnessBuilder vkeywitness multisigScript bootstrapWitness =
            { newWitnessSet
                | vkeywitness = vkeywitness
                , nativeScripts = multisigScript
                , bootstrapWitness = bootstrapWitness
            }
    in
    D.record D.int witnessBuilder <|
        D.fields
            -- vkeywitness
            >> D.optionalField 0 (D.list decodeVKeyWitness)
            -- multisig_script
            >> D.optionalField 1 (D.list decodeNativeScript)
            -- bootstrap_witness
            >> D.optionalField 2 (D.list decodeBootstrapWitness)


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


decodeNativeScript : D.Decoder NativeScript
decodeNativeScript =
    failWithMessage "decodeNativeScript failed to decode"


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


decodeAuxiliary : D.Decoder AuxiliaryData
decodeAuxiliary =
    D.map Shelley decodeMetadata


decodeMetadata : D.Decoder Metadata
decodeMetadata =
    D.dict D.int decodeMetadatum


decodeMetadatum : D.Decoder Metadatum
decodeMetadatum =
    failWithMessage "decodeMetadatum failed to decode"



-- Helper definitions


failWithMessage : String -> D.Decoder a
failWithMessage msg =
    D.raw
        |> D.andThen
            (\rawBytes ->
                let
                    _ =
                        Debug.log msg (Bytes.toString <| Bytes.fromBytes rawBytes)
                in
                D.fail
            )


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
    }


newWitnessSet : WitnessSet
newWitnessSet =
    { vkeywitness = Nothing
    , nativeScripts = Nothing
    , bootstrapWitness = Nothing
    , plutusV1Script = Nothing
    , plutusData = Nothing
    , redeemer = Nothing
    , plutusV2Script = Nothing
    }
