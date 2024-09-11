module Cardano.Gov exposing (..)

import Bytes.Comparable as Bytes exposing (Any, Bytes, width)
import Cardano.Address as Address exposing (Credential(..), CredentialHash, StakeAddress)
import Cardano.MultiAsset exposing (PolicyId)
import Cardano.Redeemer as Redeemer exposing (ExUnits)
import Cardano.Utxo exposing (TransactionId)
import Cbor.Decode as D
import Cbor.Decode.Extra as D
import Cbor.Encode as E
import Cbor.Encode.Extra as E
import Cbor.Tag as Tag
import Dict.Any exposing (AnyDict)
import Natural exposing (Natural)


type TODO
    = TODO


{-| Delegate representative.
-}
type Drep
    = DrepCredential Credential -- 0, 1
    | AlwaysAbstain -- 2
    | AlwaysNoConfidence -- 3


type Voter
    = VoterCommitteeHotCred Credential -- 0, addr_keyhash // 1, scripthash
    | VoterDrepCred Credential -- 2, addr_keyhash // 3, scripthash
    | VoterPoolId (Bytes CredentialHash) -- 4, addr_keyhash


voterFromCbor : D.Decoder Voter
voterFromCbor =
    D.int
        |> D.andThen
            (\tag ->
                case tag of
                    0 ->
                        D.map (VoterCommitteeHotCred << Address.VKeyHash) (D.map Bytes.fromBytes D.bytes)

                    1 ->
                        D.map (VoterCommitteeHotCred << Address.ScriptHash) (D.map Bytes.fromBytes D.bytes)

                    2 ->
                        D.map (VoterDrepCred << Address.VKeyHash) (D.map Bytes.fromBytes D.bytes)

                    3 ->
                        D.map (VoterDrepCred << Address.ScriptHash) (D.map Bytes.fromBytes D.bytes)

                    4 ->
                        D.map VoterPoolId (D.map Bytes.fromBytes D.bytes)

                    _ ->
                        D.failWith ("Invalid voter tag: " ++ String.fromInt tag)
            )


type Vote
    = VoteNo -- 0
    | VoteYes -- 1
    | VoteAbstain -- 2


type alias VotingProcedure =
    { vote : Vote
    , anchor : Maybe Anchor
    }


votingProcedureFromCbor : D.Decoder VotingProcedure
votingProcedureFromCbor =
    D.tuple VotingProcedure
        (D.elems
            >> D.elem
                (D.int
                    |> D.andThen
                        (\v ->
                            case v of
                                0 ->
                                    D.succeed VoteNo

                                1 ->
                                    D.succeed VoteYes

                                2 ->
                                    D.succeed VoteAbstain

                                _ ->
                                    D.failWith ("Invalid vote value: " ++ String.fromInt v)
                        )
                )
            >> D.elem (D.maybe decodeAnchor)
        )


type alias ProposalProcedure =
    { deposit : Natural
    , rewardAccount : StakeAddress
    , govAction : Action
    , anchor : Anchor
    }


proposalProcedureFromCbor : D.Decoder ProposalProcedure
proposalProcedureFromCbor =
    D.tuple ProposalProcedure
        (D.elems
            >> D.elem D.natural
            >> D.elem Address.decodeReward
            >> D.elem decodeAction
            >> D.elem decodeAnchor
        )


type Action
    = ParameterChange
        { govActionId : Maybe ActionId
        , protocolParamUpdate : ProtocolParamUpdate
        , guardrailsPolicy : Maybe (Bytes PolicyId)
        }
    | HardForkInitiation
        { govActionId : Maybe ActionId
        , protocolVersion : ProtocolVersion
        }
    | TreasuryWithdrawals
        { withdrawals : List ( StakeAddress, Natural )
        , guardrailsPolicy : Maybe (Bytes PolicyId)
        }
    | NoConfidence
        { govActionId : Maybe ActionId
        }
    | UpdateCommittee
        { govActionId : Maybe ActionId
        , removedMembers : List Credential
        , addedMembers : List { newMember : Credential, expirationEpoch : Natural }
        , quorumThreshold : UnitInterval
        }
    | NewConstitution
        { govActionId : Maybe ActionId
        , constitution : Constitution
        }
    | Info TODO


type alias Constitution =
    { anchor : Anchor
    , scripthash : Maybe (Bytes ScriptHash)
    }


type alias ScriptHash =
    CredentialHash


type alias ActionId =
    { transactionId : Bytes TransactionId
    , govActionIndex : Int
    }


actionIdFromCbor : D.Decoder ActionId
actionIdFromCbor =
    D.tuple ActionId
        (D.elems
            >> D.elem (D.map Bytes.fromBytes D.bytes)
            >> D.elem D.int
        )


decodeAnchor : D.Decoder Anchor
decodeAnchor =
    D.tuple Anchor
        (D.elems
            >> D.elem D.string
            >> D.elem (D.map Bytes.fromBytes D.bytes)
        )


decodeAction : D.Decoder Action
decodeAction =
    D.int
        |> D.andThen
            (\tag ->
                case tag of
                    0 ->
                        D.map3
                            (\govActionId update policy ->
                                ParameterChange
                                    { govActionId = govActionId
                                    , protocolParamUpdate = update
                                    , guardrailsPolicy = policy
                                    }
                            )
                            (D.maybe actionIdFromCbor)
                            decodeProtocolParamUpdate
                            (D.maybe (D.map Bytes.fromBytes D.bytes))

                    1 ->
                        D.map2
                            (\govActionId version ->
                                HardForkInitiation
                                    { govActionId = govActionId
                                    , protocolVersion = version
                                    }
                            )
                            (D.maybe actionIdFromCbor)
                            decodeProtocolVersion

                    2 ->
                        D.map2
                            (\withdrawals policy ->
                                TreasuryWithdrawals
                                    { withdrawals = withdrawals
                                    , guardrailsPolicy = policy
                                    }
                            )
                            (D.associativeList Address.decodeReward D.natural)
                            (D.maybe (D.map Bytes.fromBytes D.bytes))

                    3 ->
                        D.map
                            (\govActionId ->
                                NoConfidence { govActionId = govActionId }
                            )
                            (D.maybe actionIdFromCbor)

                    4 ->
                        D.map4
                            (\govActionId removed added threshold ->
                                UpdateCommittee
                                    { govActionId = govActionId
                                    , removedMembers = removed
                                    , addedMembers = added
                                    , quorumThreshold = threshold
                                    }
                            )
                            (D.maybe actionIdFromCbor)
                            (D.set Address.decodeCredential)
                            (D.associativeList Address.decodeCredential D.natural
                                |> D.map (List.map (\( member, epoch ) -> { newMember = member, expirationEpoch = epoch }))
                            )
                            decodeRational

                    5 ->
                        D.map2
                            (\govActionId constitution ->
                                NewConstitution
                                    { govActionId = govActionId
                                    , constitution = constitution
                                    }
                            )
                            (D.maybe actionIdFromCbor)
                            decodeConstitution

                    6 ->
                        D.succeed (Info TODO)

                    _ ->
                        D.failWith ("Invalid action tag: " ++ String.fromInt tag)
            )


decodeConstitution : D.Decoder Constitution
decodeConstitution =
    D.tuple Constitution
        (D.elems
            >> D.elem decodeAnchor
            >> D.elem (D.maybe (D.map Bytes.fromBytes D.bytes))
        )


decodeProtocolParamUpdate : D.Decoder ProtocolParamUpdate
decodeProtocolParamUpdate =
    -- TODO: Make it fail for an unknown field. Maybe use D.fold instead.
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
            -- ? 12: unit_interval      ; d. decentralization constant (deprecated)
            >> D.optionalField 12 decodeRational
            -- ? 13: $nonce             ; extra entropy (deprecated)
            >> D.optionalField 13 decodeExtraEntropy
            -- ? 14: [protocol_version] ; protocol version
            >> D.optionalField 14 decodeProtocolVersion
            -- ? 15: coin               ; min utxo value (deprecated)
            >> D.optionalField 15 D.natural
            -- ? 16: coin                ; min pool cost
            >> D.optionalField 16 D.natural
            -- ? 17: coin                ; ada per utxo byte
            >> D.optionalField 17 D.natural
            -- ? 18: costmdls            ; cost models for script languages
            >> D.optionalField 18 decodeCostModels
            -- ? 19: ex_unit_prices      ; execution costs
            >> D.optionalField 19 decodeExecutionCosts
            -- ? 20: ex_units            ; max tx ex units
            >> D.optionalField 20 Redeemer.exUnitsFromCbor
            -- ? 21: ex_units            ; max block ex units
            >> D.optionalField 21 Redeemer.exUnitsFromCbor
            -- ? 22: uint                ; max value size
            >> D.optionalField 22 D.int
            -- ? 23: uint                ; collateral percentage
            >> D.optionalField 23 D.int
            -- ? 24: uint                ; max collateral inputs
            >> D.optionalField 24 D.int
            -- Conway params
            -- poolVotingThresholds : Maybe PoolVotingThresholds -- 25
            >> D.optionalField 25 decodePoolVotingThresholds
            -- drepVotingThresholds : Maybe DrepVotingThresholds -- 26
            >> D.optionalField 26 decodeDrepVotingThresholds
            -- minCommitteeSize : Maybe Int -- 27
            >> D.optionalField 27 D.int
            -- committeeTermLimit : Maybe Natural -- 28
            >> D.optionalField 28 D.natural
            -- governanceActionValidityPeriod : Maybe Natural -- 29
            >> D.optionalField 29 D.natural
            -- governanceActionDeposit : Maybe Natural -- 30
            >> D.optionalField 30 D.natural
            -- drepDeposit : Maybe Natural -- 31
            >> D.optionalField 31 D.natural
            -- drepActivity : Maybe Natural -- 32
            >> D.optionalField 32 D.natural
            -- minFeeRefScriptCostPerByte : Maybe Int -- 33
            >> D.optionalField 33 D.int


decodePoolVotingThresholds : D.Decoder PoolVotingThresholds
decodePoolVotingThresholds =
    D.tuple PoolVotingThresholds
        (D.elems
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
        )


decodeDrepVotingThresholds : D.Decoder DrepVotingThresholds
decodeDrepVotingThresholds =
    D.tuple DrepVotingThresholds
        (D.elems
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
            >> D.elem decodeRational
        )


{-| Convenience type for `Dict` with [ActionId] keys.

WARNING: do not compare them with `==` since they contain functions.

-}
type alias ActionDict a =
    AnyDict ( String, Int ) ActionId a


type alias Anchor =
    { url : String, dataHash : Bytes AnchorDataHash }


{-| Opaque phantom type for an [Anchor] data hash.
It is 32-bytes long.
-}
type AnchorDataHash
    = AnchorDataHash


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
    , extraEntropy : Maybe Nonce -- 13 (deprecated)
    , protocolVersion : Maybe ProtocolVersion -- 14 (deprecated)
    , minUtxoValue : Maybe Natural -- 15 (deprecated)
    , minPoolCost : Maybe Natural -- 16
    , adaPerUtxoByte : Maybe Natural -- 17
    , costModelsForScriptLanguages : Maybe CostModels -- 18
    , executionCosts : Maybe ExUnitPrices -- 19
    , maxTxExUnits : Maybe ExUnits -- 20
    , maxBlockExUnits : Maybe ExUnits -- 21
    , maxValueSize : Maybe Int -- 22
    , collateralPercentage : Maybe Int -- 23
    , maxCollateralInputs : Maybe Int -- 24
    , poolVotingThresholds : Maybe PoolVotingThresholds -- 25
    , drepVotingThresholds : Maybe DrepVotingThresholds -- 26
    , minCommitteeSize : Maybe Int -- 27
    , committeeTermLimit : Maybe Natural -- 28
    , governanceActionValidityPeriod : Maybe Natural -- 29
    , governanceActionDeposit : Maybe Natural -- 30
    , drepDeposit : Maybe Natural -- 31
    , drepInactivityPeriod : Maybe Natural -- 32
    , minFeeRefScriptCostPerByte : Maybe Int -- 33
    }


{-| Default (no update) for [ProtocolParamUpdate].
-}
noParamUpdate : ProtocolParamUpdate
noParamUpdate =
    { minFeeA = Nothing -- 0
    , minFeeB = Nothing -- 1
    , maxBlockBodySize = Nothing -- 2
    , maxTransactionSize = Nothing -- 3
    , maxBlockHeaderSize = Nothing -- 4
    , keyDeposit = Nothing -- 5
    , poolDeposit = Nothing -- 6
    , maximumEpoch = Nothing -- 7
    , desiredNumberOfStakePools = Nothing -- 8
    , poolPledgeInfluence = Nothing -- 9
    , expansionRate = Nothing -- 10
    , treasuryGrowthRate = Nothing -- 11
    , decentralizationConstant = Nothing -- 12 (deprecated)
    , extraEntropy = Nothing -- 13 (deprecated)
    , protocolVersion = Nothing -- 14 (deprecated)
    , minUtxoValue = Nothing -- 15 (deprecated)
    , minPoolCost = Nothing -- 16
    , adaPerUtxoByte = Nothing -- 17
    , costModelsForScriptLanguages = Nothing -- 18
    , executionCosts = Nothing -- 19
    , maxTxExUnits = Nothing -- 20
    , maxBlockExUnits = Nothing -- 21
    , maxValueSize = Nothing -- 22
    , collateralPercentage = Nothing -- 23
    , maxCollateralInputs = Nothing -- 24
    , poolVotingThresholds = Nothing -- 25
    , drepVotingThresholds = Nothing -- 26
    , minCommitteeSize = Nothing -- 27
    , committeeTermLimit = Nothing -- 28
    , governanceActionValidityPeriod = Nothing -- 29
    , governanceActionDeposit = Nothing -- 30
    , drepDeposit = Nothing -- 31
    , drepInactivityPeriod = Nothing -- 32
    , minFeeRefScriptCostPerByte = Nothing -- 33
    }


type alias PoolVotingThresholds =
    { motionNoConfidence : UnitInterval
    , committeeNormal : UnitInterval
    , committeeNoConfidence : UnitInterval
    , hardforkInitiation : UnitInterval
    , securityRelevantParameter : UnitInterval
    }


type alias DrepVotingThresholds =
    { motionNoConfidence : UnitInterval
    , committeeNormal : UnitInterval
    , committeeNoConfidence : UnitInterval
    , updateConstitution : UnitInterval
    , hardforkInitiation : UnitInterval
    , ppNetworkGroup : UnitInterval
    , ppEconomicGroup : UnitInterval
    , ppTechnicalGroup : UnitInterval
    , ppGovernanceGroup : UnitInterval
    , treasuryWithdrawal : UnitInterval
    }


{-| -}
type alias CostModels =
    { plutusV1 : Maybe (List Int) -- 0
    , plutusV2 : Maybe (List Int) -- 1
    , plutusV3 : Maybe (List Int) -- 2
    }


{-| -}
type Nonce
    = Just0
    | RandomBytes (Bytes Any)


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



-- ENCODERS


{-| Encode [CostModels] to CBOR.
-}
encodeCostModels : CostModels -> E.Encoder
encodeCostModels =
    E.record E.int <|
        E.fields
            >> E.optionalField 0 (E.ledgerList E.int) .plutusV1
            >> E.optionalField 1 (E.ledgerList E.int) .plutusV2
            >> E.optionalField 2 (E.ledgerList E.int) .plutusV3


encodeExUnitPrices : ExUnitPrices -> E.Encoder
encodeExUnitPrices =
    E.tuple <|
        E.elems
            >> E.elem encodeRationalNumber .memPrice
            >> E.elem encodeRationalNumber .stepPrice


encodeRationalNumber : RationalNumber -> E.Encoder
encodeRationalNumber =
    E.tagged (Tag.Unknown 30) <|
        E.tuple <|
            E.elems
                >> E.elem E.int .numerator
                >> E.elem E.int .denominator


encodeDrep : Drep -> E.Encoder
encodeDrep drep =
    case drep of
        DrepCredential cred ->
            Address.credentialToCbor cred

        AlwaysAbstain ->
            E.int 2

        AlwaysNoConfidence ->
            E.int 3


encodeAnchor : Anchor -> E.Encoder
encodeAnchor =
    E.tuple
        (E.elems
            >> E.elem E.string .url
            >> E.elem Bytes.toCbor .dataHash
        )


encodePoolVotingThresholds : PoolVotingThresholds -> E.Encoder
encodePoolVotingThresholds thresholds =
    E.ledgerList encodeRationalNumber
        [ thresholds.motionNoConfidence
        , thresholds.committeeNormal
        , thresholds.committeeNoConfidence
        , thresholds.hardforkInitiation
        , thresholds.securityRelevantParameter
        ]


encodeDrepVotingThresholds : DrepVotingThresholds -> E.Encoder
encodeDrepVotingThresholds thresholds =
    E.ledgerList encodeRationalNumber
        [ thresholds.motionNoConfidence
        , thresholds.committeeNormal
        , thresholds.committeeNoConfidence
        , thresholds.updateConstitution
        , thresholds.hardforkInitiation
        , thresholds.ppNetworkGroup
        , thresholds.ppEconomicGroup
        , thresholds.ppTechnicalGroup
        , thresholds.ppGovernanceGroup
        , thresholds.treasuryWithdrawal
        ]


encodeAction : Action -> E.Encoder
encodeAction action =
    case action of
        ParameterChange { govActionId, protocolParamUpdate, guardrailsPolicy } ->
            E.ledgerList identity
                [ E.int 0
                , E.maybe encodeActionId govActionId
                , encodeProtocolParamUpdate protocolParamUpdate
                , E.maybe Bytes.toCbor guardrailsPolicy
                ]

        HardForkInitiation { govActionId, protocolVersion } ->
            E.ledgerList identity
                [ E.int 1
                , E.maybe encodeActionId govActionId
                , encodeProtocolVersion protocolVersion
                ]

        TreasuryWithdrawals { withdrawals, guardrailsPolicy } ->
            E.ledgerList identity
                [ E.int 2
                , E.ledgerAssociativeList Address.stakeAddressToCbor E.natural withdrawals
                , E.maybe Bytes.toCbor guardrailsPolicy
                ]

        NoConfidence { govActionId } ->
            E.ledgerList identity
                [ E.int 3
                , E.maybe encodeActionId govActionId
                ]

        UpdateCommittee { govActionId, removedMembers, addedMembers, quorumThreshold } ->
            E.ledgerList identity
                [ E.int 4
                , E.maybe encodeActionId govActionId
                , E.ledgerList Address.credentialToCbor removedMembers
                , E.ledgerAssociativeList Address.credentialToCbor E.natural (List.map (\m -> ( m.newMember, m.expirationEpoch )) addedMembers)
                , encodeRationalNumber quorumThreshold
                ]

        NewConstitution { govActionId, constitution } ->
            E.ledgerList identity
                [ E.int 5
                , E.maybe encodeActionId govActionId
                , encodeConstitution constitution
                ]

        Info _ ->
            E.int 6


encodeActionId : ActionId -> E.Encoder
encodeActionId =
    E.tuple
        (E.elems
            >> E.elem Bytes.toCbor .transactionId
            >> E.elem E.int .govActionIndex
        )


encodeConstitution : Constitution -> E.Encoder
encodeConstitution =
    E.tuple
        (E.elems
            >> E.elem encodeAnchor .anchor
            >> E.optionalElem Bytes.toCbor .scripthash
        )


encodeProtocolVersion : ProtocolVersion -> E.Encoder
encodeProtocolVersion ( major, minor ) =
    E.ledgerList E.int [ major, minor ]


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
            >> E.optionalField 16 E.natural .minPoolCost
            >> E.optionalField 17 E.natural .adaPerUtxoByte
            >> E.optionalField 18 encodeCostModels .costModelsForScriptLanguages
            >> E.optionalField 19 encodeExUnitPrices .executionCosts
            >> E.optionalField 20 Redeemer.encodeExUnits .maxTxExUnits
            >> E.optionalField 21 Redeemer.encodeExUnits .maxBlockExUnits
            >> E.optionalField 22 E.int .maxValueSize
            >> E.optionalField 23 E.int .collateralPercentage
            >> E.optionalField 24 E.int .maxCollateralInputs
            -- Conway fields
            >> E.optionalField 25 encodePoolVotingThresholds .poolVotingThresholds
            >> E.optionalField 26 encodeDrepVotingThresholds .drepVotingThresholds
            >> E.optionalField 27 E.int .minCommitteeSize
            >> E.optionalField 28 E.natural .committeeTermLimit
            >> E.optionalField 29 E.natural .governanceActionValidityPeriod
            >> E.optionalField 30 E.natural .governanceActionDeposit
            >> E.optionalField 31 E.natural .drepDeposit
            >> E.optionalField 32 E.natural .drepInactivityPeriod
            >> E.optionalField 33 E.int .minFeeRefScriptCostPerByte


encodeVoter : Voter -> E.Encoder
encodeVoter voter =
    case voter of
        VoterCommitteeHotCred cred ->
            case cred of
                Address.VKeyHash hash ->
                    E.ledgerList identity [ E.int 0, Bytes.toCbor hash ]

                Address.ScriptHash hash ->
                    E.ledgerList identity [ E.int 1, Bytes.toCbor hash ]

        VoterDrepCred cred ->
            case cred of
                Address.VKeyHash hash ->
                    E.ledgerList identity [ E.int 2, Bytes.toCbor hash ]

                Address.ScriptHash hash ->
                    E.ledgerList identity [ E.int 3, Bytes.toCbor hash ]

        VoterPoolId poolId ->
            E.ledgerList identity [ E.int 4, Bytes.toCbor poolId ]


encodeVotingProcedure : VotingProcedure -> E.Encoder
encodeVotingProcedure =
    E.tuple
        (E.elems
            >> E.elem encodeVote .vote
            >> E.elem (E.maybe encodeAnchor) .anchor
        )


encodeVote : Vote -> E.Encoder
encodeVote vote =
    case vote of
        VoteNo ->
            E.int 0

        VoteYes ->
            E.int 1

        VoteAbstain ->
            E.int 2



-- DECODERS


decodeCostModels : D.Decoder CostModels
decodeCostModels =
    -- TODO: Make it fail for an unknown field. Maybe use D.fold instead.
    D.record D.int (\v1costs v2costs v3costs -> { plutusV1 = v1costs, plutusV2 = v2costs, plutusV3 = v3costs }) <|
        D.fields
            -- plutusV1
            >> D.optionalField 0 (D.list D.int)
            -- plutusV2
            >> D.optionalField 1 (D.list D.int)
            -- plutusV3
            >> D.optionalField 2 (D.list D.int)


decodeExtraEntropy : D.Decoder Nonce
decodeExtraEntropy =
    D.length
        |> D.andThen
            (\l ->
                -- $nonce /= [ 0 // 1, bytes .size 32 ]
                case l of
                    1 ->
                        -- Remark: we don’t check that the value is 0
                        -- We just assume its correct and do not validate.
                        D.map (always Just0) D.int

                    2 ->
                        -- Remark: we don’t check that the value is 1
                        -- We just assume its correct and do not validate.
                        D.int |> D.ignoreThen (D.map (RandomBytes << Bytes.fromBytes) D.bytes)

                    _ ->
                        D.fail
            )


decodeProtocolVersion : D.Decoder ProtocolVersion
decodeProtocolVersion =
    D.tuple Tuple.pair <|
        D.elems
            >> D.elem D.int
            >> D.elem D.int


decodeExecutionCosts : D.Decoder ExUnitPrices
decodeExecutionCosts =
    D.tuple ExUnitPrices <|
        D.elems
            >> D.elem decodeRational
            >> D.elem decodeRational


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


decodeDrep : D.Decoder Drep
decodeDrep =
    D.length
        |> D.ignoreThen D.int
        |> D.andThen
            (\tag ->
                case tag of
                    0 ->
                        D.map (DrepCredential << Address.VKeyHash) (D.map Bytes.fromBytes D.bytes)

                    1 ->
                        D.map (DrepCredential << Address.ScriptHash) (D.map Bytes.fromBytes D.bytes)

                    2 ->
                        D.succeed AlwaysAbstain

                    3 ->
                        D.succeed AlwaysNoConfidence

                    _ ->
                        D.failWith ("Invalid Drep tag: " ++ String.fromInt tag)
            )
