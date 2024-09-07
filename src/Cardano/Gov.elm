module Cardano.Gov exposing (..)

import Bytes.Comparable as Bytes exposing (Any, Bytes)
import Cardano.Address as Address exposing (Credential, CredentialHash, StakeAddress)
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


type Vote
    = VoteNo -- 0
    | VoteYes -- 1
    | VoteAbstain -- 2


type alias VotingProcedure =
    { vote : Vote, anchor : Maybe Anchor }


type alias ProposalProcedure =
    { deposit : Natural
    , rewardAccount : StakeAddress
    , govAction : Action
    , anchor : Anchor
    }


type Action
    = ParameterChange
        { govActionId : Maybe ActionId
        , protocolParamUpdate : ProtocolParamUpdate
        , guardrailsPolicy : Maybe (Bytes PolicyId)
        }
    | HardForkInitiation TODO
    | TreasuryWithdrawals TODO
    | NoConfidence TODO
    | UpdateCommittee TODO
    | NewConstitution TODO
    | Info TODO


type alias ActionId =
    { transactionId : Bytes TransactionId
    , govActionIndex : Int
    }


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
    , protocolVersion : Maybe ProtocolVersion -- 14
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
    }


{-| -}
type alias CostModels =
    { plutusV1 : Maybe (List Int) -- 0
    , plutusV2 : Maybe (List Int) -- 1
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



-- DECODERS


decodeCostModels : D.Decoder CostModels
decodeCostModels =
    -- TODO: Make it fail for an unknown field. Maybe use D.fold instead.
    D.record D.int (\v1costs v2costs -> { plutusV1 = v1costs, plutusV2 = v2costs }) <|
        D.fields
            -- plutusV1
            >> D.optionalField 0 (D.list D.int)
            -- plutusV2
            >> D.optionalField 1 (D.list D.int)


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
