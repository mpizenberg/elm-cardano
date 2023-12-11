module Cardano.Transaction exposing
    ( Transaction
    , TransactionBody, AuxiliaryDataHash, ScriptDataHash
    , WitnessSet
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

import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Bytes.Map exposing (BytesMap)
import Cardano.Address as Address exposing (Credential, CredentialHash, NetworkId, StakeAddress)
import Cardano.Data as Data exposing (Data)
import Cardano.MultiAsset as MultiAsset exposing (MultiAsset, PolicyId)
import Cardano.Redeemer as Redeemer exposing (ExUnits, Redeemer)
import Cardano.Script as Script exposing (NativeScript, ScriptCbor)
import Cardano.Transaction.AuxiliaryData as AuxiliaryData exposing (AuxiliaryData)
import Cardano.Utxo as Utxo exposing (Output, OutputReference, encodeOutput, encodeOutputReference)
import Cbor.Decode as D
import Cbor.Decode.Extra as D
import Cbor.Encode as E
import Cbor.Encode.Extra as E
import Cbor.Tag as Tag
import Natural exposing (Natural)


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
    , fee : Maybe Natural -- 2
    , ttl : Maybe Natural -- 3
    , certificates : List Certificate -- 4
    , withdrawals : List ( StakeAddress, Natural ) -- 5
    , update : Maybe Update -- 6
    , auxiliaryDataHash : Maybe (Bytes AuxiliaryDataHash) -- 7
    , validityIntervalStart : Maybe Int -- 8

    -- TODO: Use a type that can have negative numbers instead
    -- and make MultiAsset only positive numbers?
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


{-| Payload to update the protocol parameters at a specific epoch
-}
type alias Update =
    { proposedProtocolParameterUpdates : BytesMap GenesisHash ProtocolParamUpdate
    , epoch : Natural
    }


{-| Adjustable parameters that power key aspects of the network.
-}
type alias ProtocolParamUpdate =
    { minFeeA : Maybe Natural -- 0
    , minFeeB : Maybe Natural -- 1
    , maxBlockBodySize : Maybe Int -- 2
    , maxTransactionSize : Maybe Int -- 3
    , maxBlockHeaderSize : Maybe Int -- 4
    , keyDeposit : Maybe Natural -- 5
    , poolDeposit : Maybe Natural -- 6
    , maximumEpoch : Maybe Natural -- 7
    , desiredNumberOfStakePools : Maybe Int -- 8
    , poolPledgeInfluence : Maybe RationalNumber -- 9
    , expansionRate : Maybe UnitInterval -- 10
    , treasuryGrowthRate : Maybe UnitInterval -- 11
    , decentralizationConstant : Maybe UnitInterval -- 12 (deprecated)
    , extraEntropy : Maybe ( Int, Bytes Any ) -- 13 (deprecated)
    , protocolVersion : Maybe ProtocolVersion -- 14
    , minUtxoValue : Maybe Natural -- 15 (deprecated)
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
type alias VKeyWitness =
    { vkey : Bytes Ed25519PublicKey -- 0
    , signature : Bytes Ed25519Signature -- 1
    }


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
    | PoolRetirement { poolId : Bytes PoolId, epoch : Natural }
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
    bytes
        |> Bytes.toBytes
        |> D.decode (D.oneOf [ decodeTransaction, D.failWith "Transaction decoder failed" ])


{-| -}
encodeTransaction : Transaction -> E.Encoder
encodeTransaction =
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
            >> E.optionalField 2 E.natural .fee
            >> E.optionalField 3 E.natural .ttl
            >> E.nonEmptyField 4 List.isEmpty encodeCertificates .certificates
            >> E.nonEmptyField 5 List.isEmpty (E.ledgerAssociativeList Address.stakeAddressToCbor E.natural) .withdrawals
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
                , E.natural poolParams.pledge
                , E.natural poolParams.cost
                , encodeRationalNumber poolParams.margin
                , Address.stakeAddressToCbor poolParams.rewardAccount
                , E.ledgerList Bytes.toCbor poolParams.poolOwners
                , E.ledgerList encodeRelay poolParams.relays
                , E.maybe encodePoolMetadata poolParams.poolMetadata
                ]

            PoolRetirement { poolId, epoch } ->
                [ E.int 4
                , Bytes.toCbor poolId
                , E.natural epoch
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
            E.ledgerAssociativeList Address.credentialToCbor E.natural distribution

        OtherAccountingPot n ->
            E.natural n


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
            >> E.elem E.natural .epoch


{-| -}
encodeProposedProtocolParameterUpdates : BytesMap GenesisHash ProtocolParamUpdate -> E.Encoder
encodeProposedProtocolParameterUpdates =
    Bytes.Map.toCbor encodeProtocolParamUpdate


encodeProtocolParamUpdate : ProtocolParamUpdate -> E.Encoder
encodeProtocolParamUpdate =
    E.record E.int <|
        E.fields
            >> E.optionalField 0 E.natural .minFeeA
            >> E.optionalField 1 E.natural .minFeeB
            >> E.optionalField 2 E.int .maxBlockBodySize
            >> E.optionalField 3 E.int .maxTransactionSize
            >> E.optionalField 4 E.int .maxBlockHeaderSize
            >> E.optionalField 5 E.natural .keyDeposit
            >> E.optionalField 6 E.natural .poolDeposit
            >> E.optionalField 7 E.natural .maximumEpoch
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
            >> D.elem (D.oneOf [ decodeBody, D.failWith "Failed to decode body" ])
            >> D.elem (D.oneOf [ decodeWitness, D.failWith "Failed to decode witness" ])
            >> D.elem (D.oneOf [ D.maybe AuxiliaryData.fromCbor, D.failWith "Failed to decode auxiliary" ])



-- Decode body


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
            >> D.field 2 D.natural
            -- ttl
            >> D.field 3 D.natural
            -- certificates
            >> D.optionalField 4 (D.oneOf [ D.list decodeCertificate, D.failWith "Failed to decode certificate" ])
            -- withdrawals
            >> D.optionalField 5 (D.oneOf [ decodeWithdrawals, D.failWith "Failed to decode withdrawals" ])
            -- update
            >> D.optionalField 6 decodeUpdate
            -- metadata hash
            >> D.optionalField 7 (D.map Bytes.fromBytes D.bytes)


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
            D.map (\cred -> StakeRegistration { delegator = cred }) decodeStakeCredential

        -- stake_deregistration = (1, stake_credential)
        ( 2, 1 ) ->
            D.map (\cred -> StakeDeregistration { delegator = cred }) decodeStakeCredential

        -- stake_delegation = (2, stake_credential, pool_keyhash)
        ( 3, 2 ) ->
            D.map2
                (\cred poolId -> StakeDelegation { delegator = cred, poolId = poolId })
                decodeStakeCredential
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

        _ ->
            D.failWith <|
                "Unknown length and id for certificate ("
                    ++ String.fromInt length
                    ++ ", "
                    ++ String.fromInt id
                    ++ ")"


decodeStakeCredential : D.Decoder Credential
decodeStakeCredential =
    D.length
        |> D.andThen
            (\length ->
                -- A stake credential contains 2 elements
                if length == 2 then
                    D.int
                        |> D.andThen
                            (\id ->
                                if id == 0 then
                                    -- If the id is 0, it's a vkey hash
                                    D.map (Address.VKeyHash << Bytes.fromBytes) D.bytes

                                else if id == 1 then
                                    -- If the id is 1, it's a script hash
                                    D.map (Address.ScriptHash << Bytes.fromBytes) D.bytes

                                else
                                    D.fail
                            )

                else
                    D.fail
            )


decodePoolParams : D.Decoder PoolParams
decodePoolParams =
    D.succeed PoolParams
        |> D.keep (D.oneOf [ D.map Bytes.fromBytes D.bytes, D.failWith "Failed to decode operator" ])
        |> D.keep (D.oneOf [ D.map Bytes.fromBytes D.bytes, D.failWith "Failed to decode vrfkeyhash" ])
        |> D.keep (D.oneOf [ D.natural, D.failWith "Failed to decode pledge" ])
        |> D.keep D.natural
        |> D.keep (D.oneOf [ decodeRational, D.failWith "Failed to decode rational" ])
        |> D.keep (D.oneOf [ Address.decodeReward, D.failWith "Failed to decode reward" ])
        |> D.keep (D.list (D.map Bytes.fromBytes D.bytes))
        |> D.keep (D.list <| D.oneOf [ decodeRelay, D.failWith "Failed to decode Relay" ])
        |> D.keep (D.oneOf [ D.maybe decodePoolMetadata, D.failWith "Failed to decode pool metadata" ])


decodeRational : D.Decoder RationalNumber
decodeRational =
    D.tag
        |> D.andThen
            (\tag ->
                case tag of
                    Tag.Unknown 30 ->
                        D.tuple RationalNumber <|
                            D.elems
                                >> D.elem D.int
                                >> D.elem D.int

                    _ ->
                        D.fail
            )


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
            >> D.elem (D.associativeList decodeStakeCredential D.natural)


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
            >> D.elem (D.list <| D.map2 Tuple.pair (D.map Bytes.fromBytes D.bytes) decodeProtocolParamUpdate)
            >> D.elem D.natural


decodeProtocolParamUpdate : D.Decoder ProtocolParamUpdate
decodeProtocolParamUpdate =
    D.record D.int ProtocolParamUpdate <|
        D.fields
            -- ? 0:  uint               ; minfee A
            >> D.optionalField 0 D.natural
            -- ? 1:  uint               ; minfee B
            >> D.optionalField 1 D.natural
            -- ? 2:  uint               ; max block body size
            >> D.optionalField 2 D.int
            -- ? 3:  uint               ; max transaction size
            >> D.optionalField 3 D.int
            -- ? 4:  uint               ; max block header size
            >> D.optionalField 4 D.int
            -- ? 5:  coin               ; key deposit
            >> D.optionalField 5 D.natural
            -- ? 6:  coin               ; pool deposit
            >> D.optionalField 6 D.natural
            -- ? 7: epoch               ; maximum epoch
            >> D.optionalField 7 D.natural
            -- ? 8: uint                ; n_opt: desired number of stake pools
            >> D.optionalField 8 D.int
            -- ? 9: rational            ; pool pledge influence
            >> D.optionalField 9 decodeRational
            -- ? 10: unit_interval      ; expansion rate
            >> D.optionalField 10 decodeRational
            -- ? 11: unit_interval      ; treasury growth rate
            >> D.optionalField 11 decodeRational
            -- ? 12: unit_interval      ; d. decentralization constant
            >> D.optionalField 12 decodeRational
            -- ? 13: $nonce             ; extra entropy
            >> D.optionalField 13 decodeExtraEntropy
            -- ? 14: [protocol_version] ; protocol version
            >> D.optionalField 14 decodeProtocolVersion
            -- ? 15: coin               ; min utxo value
            >> D.optionalField 15 D.natural
            >> D.optionalField 16 (D.failWith "minPoolCost")
            >> D.optionalField 17 (D.failWith "adaPerUtxoByte")
            >> D.optionalField 18 (D.failWith "costModelsForScriptLanguages")
            >> D.optionalField 19 (D.failWith "executionCosts")
            >> D.optionalField 20 (D.failWith "maxTxExUnits")
            >> D.optionalField 21 (D.failWith "maxBlockExUnits")
            >> D.optionalField 22 (D.failWith "maxValueSize")
            >> D.optionalField 23 (D.failWith "collateralPercentage")
            >> D.optionalField 24 (D.failWith "maxCollateralInputs")


decodeExtraEntropy : D.Decoder ( Int, Bytes Any )
decodeExtraEntropy =
    D.tuple Tuple.pair <|
        D.elems
            >> D.elem D.int
            >> D.elem (D.map Bytes.fromBytes D.bytes)


decodeProtocolVersion : D.Decoder ProtocolVersion
decodeProtocolVersion =
    D.tuple Tuple.pair <|
        D.elems
            >> D.elem D.int
            >> D.elem D.int



-- Decode witness


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
    D.failWith "decodeNativeScript (not implemented) failed to decode"


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



-- Helper definitions


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
