module ElmCardano.Transaction exposing
    ( Transaction
    , TransactionBody, WitnessSet
    , Value(..), PolicyId, adaAssetName
    , Address, Credential(..), StakeCredential(..)
    , Input, OutputReference, Output(..)
    , ScriptContext, ScriptPurpose(..)
    , Certificate(..)
    , DatumOption(..), Metadatum(..), NativeScript(..), Redeemer, RedeemerTag(..), Script(..), fromCbor, toCbor
    )

{-| Types and functions related to on-chain transactions.

@docs Transaction

@docs TransactionBody, WitnessSet

@docs Value, MintedValue, PolicyId, adaAssetName

@docs Address, Credential, StakeCredential

@docs Datum, Input, OutputReference, Output

@docs ScriptContext, ScriptPurpose

@docs Certificate

-}

import Bytes exposing (Bytes)
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Encode.Extra as E
import Cbor.Tag exposing (Tag(..))
import Debug exposing (todo)
import ElmCardano.Core exposing (Coin, Data(..), NetworkId(..))
import ElmCardano.Hash exposing (Blake2b_224, Blake2b_256)
import ElmCardano.KeyValuePair as KeyValuePair exposing (KeyValuePair)


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
    { inputs : List Input -- 0
    , outputs : List Output -- 1
    , fee : Maybe Int -- 2
    , ttl : Maybe Int -- 3
    , certificates : List Certificate -- 4
    , withdrawals : KeyValuePair RewardAccount Coin -- 5
    , update : Maybe Update -- 6
    , auxiliaryDataHash : Maybe Blake2b_256 -- 7
    , validityIntervalStart : Maybe Int -- 8
    , mint : Multiasset Coin -- 9
    , scriptDataHash : Maybe Bytes -- 11
    , collateral : List Input -- 13
    , requiredSigners : List Blake2b_224 -- 14
    , networkId : Maybe NetworkId -- 15
    , collateralReturn : Maybe Output -- 16
    , totalCollateral : Maybe Coin -- 17
    , referenceInputs : List Input -- 18
    }


{-| A Cardano transaction witness set.
<https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L763>
-}
type alias WitnessSet =
    { vkeywitness : Maybe (List VKeyWitness) -- 0
    , nativeScripts : Maybe (List NativeScript) -- 1
    , bootstrapWitness : Maybe (List BootstrapWitness) -- 2
    , plutusV1Script : Maybe (List PlutusV1Script) -- 3
    , plutusData : Maybe (List Data) -- 4
    , redeemer : Maybe (List Redeemer) -- 5
    , plutusV2Script : Maybe (List PlutusV2Script) -- 6
    }


type alias AuxiliaryData =
    { metadata : Maybe Metadata -- 0
    , nativeScripts : Maybe (List NativeScript) -- 1
    , plutusScripts : Maybe (List PlutusScript) -- 1
    }


type alias Multiasset a =
    KeyValuePair PolicyId (KeyValuePair AssetName a)


type alias Update =
    { proposedProtocolParameterUpdates : KeyValuePair GenesisHash ProtocolParamUpdate
    , epoch : Epoch
    }


type alias ProtocolParamUpdate =
    { -- #[n(0)]
      minfeeA : Maybe Int
    , -- #[n(1)]
      minfeeB : Maybe Int
    , -- #[n(2)]
      maxBlockBodySize : Maybe Int
    , -- #[n(3)]
      maxTransactionSize : Maybe Int
    , -- #[n(4)]
      maxBlockHeaderSize : Maybe Int
    , -- #[n(5)]
      keyDeposit : Maybe Coin
    , -- #[n(6)]
      poolDeposit : Maybe Coin
    , -- #[n(7)]
      maximumEpoch : Maybe Epoch
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
      minPoolCost : Maybe Coin
    , -- #[n(17)]
      adaPerUtxoByte : Maybe Coin
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


type alias CostModels =
    { -- #[n(0)]
      plutusV1 : Maybe CostModel
    , -- #[n(1)]
      plutusV2 : Maybe CostModel
    }


type alias CostModel =
    List Int


type alias ExUnitPrices =
    { -- #[n(0)]
      memPrice : PositiveInterval
    , -- #[n(1)]
      stepPrice : PositiveInterval
    }


type alias ProtocolVersion =
    ( Int, Int )


type alias UnitInterval =
    RationalNumber


type alias PositiveInterval =
    RationalNumber



-- https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L379


type alias RationalNumber =
    { numerator : Int
    , denominator : Int
    }


type alias GenesisHash =
    Bytes


type alias Epoch =
    Int


type alias Metadata =
    KeyValuePair MetadatumLabel Metadatum


type alias MetadatumLabel =
    Int


type Metadatum
    = MInt Int
    | MBytes Bytes
    | Text String
    | MList (List Metadatum)
    | Map (KeyValuePair Metadatum Metadatum)


type alias RewardAccount =
    Bytes


type alias PlutusScript =
    Bytes


type alias PlutusV1Script =
    Bytes


type alias PlutusV2Script =
    Bytes



-- script = [ 0, native_script // 1, plutus_v1_script // 2, plutus_v2_script ]


{-| <https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/babbage/model.rs#L58>
-}
type Script
    = Native NativeScript
    | PlutusV1 PlutusV1Script
    | PlutusV2 PlutusV2Script


type alias VKeyWitness =
    { vkey : Bytes -- 0
    , signature : Bytes
    }


{-| A native script
<https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L772>
-}
type NativeScript
    = ScriptPubkey
        -- TODO: AddrKeyHash
        Bytes
    | ScriptAll (List NativeScript)
    | ScriptAny (List NativeScript)
    | ScriptNofK Int (List NativeScript)
    | InvalidBefore Int
    | InvalidHereafter Int


type alias Redeemer =
    { tag : RedeemerTag -- 0
    , index : Int -- 1
    , data : Data -- 2
    , exUnits : ExUnits -- 3
    }


type RedeemerTag
    = Spend
    | Mint
    | Cert
    | Reward


type alias ExUnits =
    { mem : Int -- 0
    , steps : Int -- 1
    }



-- TODO: what kinds of hashes are these?


type alias BootstrapWitness =
    { publicKey : Bytes -- 0
    , signature : Bytes -- 1
    , chainCode : Bytes -- 2
    , attributes : Bytes -- 3
    }



-- Token Values ################################################################


{-| A multi-asset output Value. Contains tokens indexed by policy id and asset name.

This type maintains some invariants by construction.
In particular, a Value will never contain a zero quantity of a particular token.

-}
type Value
    = Coin Coin
    | Multiasset Coin (Multiasset Coin)


{-| The policy id of a Cardano Token. Ada ("") is a special case since it cannot be minted.
-}
type alias PolicyId =
    Blake2b_224


type alias AssetName =
    Bytes


{-| Ada, the native currency, isn’t associated with any AssetName (it’s not possible to mint Ada!).
By convention, it is an empty ByteArray.
-}
adaAssetName : String
adaAssetName =
    ""



-- Credentials #################################################################


{-| A Cardano address typically holding one or two credential references.

Note that legacy bootstrap addresses (a.k.a. "Byron addresses") are completely excluded from Plutus contexts.
Thus, from an on-chain perspective only exists addresses of type 00, 01, …, 07 as detailed in CIP-0019 :: Shelley Addresses.

-}
type alias Address =
    { paymentCredential : Credential
    , stakeCredential : Maybe StakeCredential
    }


{-| A general structure for representing an on-chain credential.

Credentials are always one of two kinds: a direct public/private key pair, or a script (native or Plutus).

-}
type Credential
    = VerificationKeyCredential Blake2b_224
    | ScriptCredential Blake2b_224


{-| A StakeCredential represents the delegation and rewards withdrawal conditions associated with some stake address / account.

A StakeCredential is either provided inline, or, by reference using an on-chain pointer.
Read more about pointers in CIP-0019 :: Pointers.

-}
type StakeCredential
    = InlineCredential Credential
    | PointerCredential { slotNumber : Int, transactionIndex : Int, certificateIndex : Int }



-- Inputs/Outputs ##############################################################


{-| Nickname for data stored in a eUTxO.
-}
type DatumOption
    = DatumHash Blake2b_256
    | Datum Data


{-| An input eUTxO for a transaction.
-}
type alias Input =
    { transactionId : Blake2b_256
    , outputIndex : Int
    }


{-| The reference for a eUTxO.
-}
type alias OutputReference =
    Input


{-| The content of a eUTxO.
-}
type Output
    = Legacy
        { address : Bytes
        , amount : Value
        , datumHash : Maybe Bytes
        }
    | PostAlonzo
        { address : Bytes
        , value : Value
        , datumOption : Maybe DatumOption
        , referenceScript : Maybe Blake2b_224
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
    = SPMint PolicyId
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
    | CredentialDelegation { delegator : StakeCredential, delegatee : Blake2b_224 }
    | PoolRegistration { poolId : Blake2b_224, vrf : Blake2b_224 }
    | PoolDeregistration { poolId : Blake2b_224, epoch : Int }
    | Governance
    | TreasuryMovement



-- https://github.com/input-output-hk/cardano-ledger/blob/a792fbff8156773e712ef875d82c2c6d4358a417/eras/babbage/test-suite/cddl-files/babbage.cddl#L13


toCbor : Transaction -> Bytes
toCbor tx =
    tx |> encodeTransaction |> E.encode


fromCbor : Bytes -> Maybe Transaction
fromCbor bytes =
    D.decode decodeTransaction bytes


encodeTransaction : Transaction -> E.Encoder
encodeTransaction { body, witnessSet, isValid, auxiliaryData } =
    E.list identity
        [ encodeTransactionBody body
        , encodeWitnessSet witnessSet
        , E.bool isValid
        , encodeNullable encodeAuxiliaryData auxiliaryData
        ]


encodeTransactionBody : TransactionBody -> E.Encoder
encodeTransactionBody =
    E.record E.int <|
        E.fields
            >> E.field 0 encodeInputs .inputs
            >> E.field 1 encodeOutputs .outputs
            >> E.optionalField 2 E.int .fee
            >> E.optionalField 3 E.int .ttl
            >> E.nonEmptyField 4 List.isEmpty encodeCertificates .certificates
            >> E.nonEmptyField 5 KeyValuePair.isEmpty (\_ -> todo "KeyValuePair.toCbor") .withdrawals
            >> E.optionalField 6 (\_ -> todo "Update.toCbor") .update
            >> E.optionalField 7 E.bytes .auxiliaryDataHash
            >> E.optionalField 8 E.int .validityIntervalStart
            >> E.nonEmptyField 9 KeyValuePair.isEmpty (\_ -> todo "Multiasset.toCbor") .mint
            >> E.optionalField 11 E.bytes .scriptDataHash
            >> E.nonEmptyField 13 List.isEmpty encodeInputs .collateral
            >> E.nonEmptyField 14 List.isEmpty encodeRequiredSigners .requiredSigners
            >> E.optionalField 15 encodeNetworkId .networkId
            >> E.optionalField 16 encodeOutput .collateralReturn
            >> E.optionalField 17 E.int .totalCollateral
            >> E.nonEmptyField 18 List.isEmpty encodeInputs .referenceInputs


encodeNetworkId : NetworkId -> E.Encoder
encodeNetworkId networkId =
    E.int <|
        case networkId of
            Testnet ->
                0

            Mainnet ->
                1


encodeWitnessSet : WitnessSet -> E.Encoder
encodeWitnessSet =
    E.record E.int <|
        E.fields
            >> E.optionalField 0 encodeVKeyWitnesses .vkeywitness
            >> E.optionalField 1 (\_ -> todo "") .nativeScripts
            >> E.optionalField 2 encodeBootstrapWitnesses .bootstrapWitness
            >> E.optionalField 3 (\scripts -> E.list E.bytes scripts) .plutusV1Script
            >> E.optionalField 4 (encodeList encodeData) .plutusData
            >> E.optionalField 5 (\redeemers -> E.list encodeRedeemer redeemers) .redeemer
            >> E.optionalField 6 (\scripts -> E.list E.bytes scripts) .plutusV2Script


encodeVKeyWitnesses : List VKeyWitness -> E.Encoder
encodeVKeyWitnesses v =
    E.list encodeVKeyWitness v


encodeVKeyWitness : VKeyWitness -> E.Encoder
encodeVKeyWitness v =
    E.list E.bytes
        [ v.vkey
        , v.signature
        ]


encodeBootstrapWitnesses : List BootstrapWitness -> E.Encoder
encodeBootstrapWitnesses b =
    E.list encodeBootstrapWitness b


encodeBootstrapWitness : BootstrapWitness -> E.Encoder
encodeBootstrapWitness b =
    E.list E.bytes
        [ b.publicKey
        , b.signature
        ]


encodeAuxiliaryData : AuxiliaryData -> E.Encoder
encodeAuxiliaryData _ =
    todo "encode auxiliary data"


encodeInputs : List Input -> E.Encoder
encodeInputs inputs =
    E.list encodeInput inputs


encodeInput : Input -> E.Encoder
encodeInput { transactionId, outputIndex } =
    E.list identity
        [ E.bytes transactionId
        , E.int outputIndex
        ]


encodeOutputs : List Output -> E.Encoder
encodeOutputs outputs =
    E.list encodeOutput outputs


encodeOutput : Output -> E.Encoder
encodeOutput output =
    case output of
        Legacy { address, amount, datumHash } ->
            let
                optionalDatumHash =
                    case datumHash of
                        Just h ->
                            [ E.bytes h ]

                        Nothing ->
                            []
            in
            E.list identity
                ([ E.bytes address
                 , encodeValue amount
                 ]
                    ++ optionalDatumHash
                )

        PostAlonzo fields ->
            let
                encodePostAlonzo =
                    E.record E.int <|
                        E.fields
                            >> E.field 0 E.bytes .address
                            >> E.field 1 encodeValue .value
                            >> E.optionalField 2 encodeDatumOption .datumOption
                            >> E.optionalField 3 (\_ -> todo "") .referenceScript
            in
            encodePostAlonzo fields


encodeDatumOption : DatumOption -> E.Encoder
encodeDatumOption datumOption =
    E.list identity <|
        case datumOption of
            DatumHash hash ->
                [ E.int 0
                , E.bytes hash
                ]

            Datum datum ->
                [ E.int 1
                , datum
                    |> encodeData
                    |> E.encode
                    |> E.tagged Cbor E.bytes
                ]


encodeData : Data -> E.Encoder
encodeData data =
    case data of
        Constr { tag, fields } ->
            let
                -- TODO: we probably don't need to do this
                -- I (rvcas) only did this to match the tx in the unit test
                encoder =
                    if List.length fields == 0 then
                        E.list encodeData

                    else
                        encodeList encodeData
            in
            E.tagged (Unknown tag) encoder fields

        BData bdata ->
            E.bytes bdata


encodeList : (a -> E.Encoder) -> List a -> E.Encoder
encodeList encoder =
    List.map encoder
        >> List.foldr (::) [ E.break ]
        >> (::) E.beginList
        >> E.sequence


encodeRedeemer : Redeemer -> E.Encoder
encodeRedeemer { tag, index, data, exUnits } =
    E.list identity
        [ encodeRedeemerTag tag
        , E.int index
        , encodeData data
        , encodeExUnits exUnits
        ]


encodeRedeemerTag : RedeemerTag -> E.Encoder
encodeRedeemerTag redeemerTag =
    E.int <|
        case redeemerTag of
            Spend ->
                0

            Mint ->
                1

            Cert ->
                2

            Reward ->
                3


encodeExUnits : ExUnits -> E.Encoder
encodeExUnits exUnits =
    E.list E.int
        [ exUnits.mem
        , exUnits.steps
        ]


encodeCertificates : List Certificate -> E.Encoder
encodeCertificates certificates =
    E.list encodeCertificate certificates


encodeCertificate : Certificate -> E.Encoder
encodeCertificate _ =
    todo "encode certificate"


encodeRequiredSigners : List Bytes -> E.Encoder
encodeRequiredSigners requiredSigners =
    E.list E.bytes requiredSigners


encodeValue : Value -> E.Encoder
encodeValue value =
    case value of
        Coin amount ->
            E.int amount

        Multiasset amount _ ->
            E.sequence
                [ E.beginList
                , E.int amount
                , todo "encode multiasset"
                , E.break
                ]


{-| Encode things shown in the cddl as `x // null`.
-}
encodeNullable : (a -> E.Encoder) -> Maybe a -> E.Encoder
encodeNullable apply value =
    encodeMaybe apply E.null value


encodeMaybe : (a -> E.Encoder) -> E.Encoder -> Maybe a -> E.Encoder
encodeMaybe apply default value =
    value
        |> Maybe.map apply
        |> Maybe.withDefault default


decodeTransaction : D.Decoder Transaction
decodeTransaction =
    todo "decode tx"
