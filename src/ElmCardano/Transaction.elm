module ElmCardano.Transaction exposing
    ( Transaction
    , Value, MintedValue, PolicyId(..), adaAssetName
    , Address, Credential(..), StakeCredential(..)
    , Datum(..), Input, OutputReference, Output
    , ScriptContext, ScriptPurpose(..)
    , Certificate(..)
    )

{-| Types and functions related to on-chain transactions.

@docs Transaction

@docs Value, MintedValue, PolicyId, adaAssetName

@docs Address, Credential, StakeCredential

@docs Datum, Input, OutputReference, Output

@docs ScriptContext, ScriptPurpose

@docs Certificate

-}

import Bytes exposing (Bytes)
import Cbor.Decode as D
import Cbor.Encode as E
import Debug exposing (todo)
import ElmCardano.Core exposing (Data, PosixTime)
import ElmCardano.Hash exposing (Blake2b_224, Blake2b_256, Hash)
import ElmCardano.Interval exposing (Interval)


{-| A Cardano transaction.
-}
type alias Transaction =
    { inputs : List Input -- Transaction inputs are sorted by address
    , referenceInputs : List Input -- Transaction inputs are sorted by address
    , outputs : List Output -- Transaction outputs are not sorted
    , fee : Value
    , mint : MintedValue
    , certificates : List Certificate
    , withdrawals : List ( StakeCredential, Int ) -- Aiken uses a Dict
    , validityRange : Interval PosixTime
    , extraSignatories : List (Hash Blake2b_224)
    , redeemers : List ( ScriptPurpose, Data ) -- Aiken uses a Dict
    , datums : List ( Hash Blake2b_256, Data ) -- Aiken uses a Dict
    , id : { hash : Hash Blake2b_256 }
    }



-- Token Values ################################################################


{-| A multi-asset output Value. Contains tokens indexed by policy id and asset name.

This type maintains some invariants by construction.
In particular, a Value will never contain a zero quantity of a particular token.

-}
type Value
    = Value (List { policyId : PolicyId, assetName : String, amount : Int })


{-| A multi-asset value that can be found when minting transaction.

Note that because of historical reasons, this is slightly different from Value found in transaction outputs.

-}
type MintedValue
    = MintedValue


{-| The policy id of a Cardano Token. Ada is a special case since it cannot be minted.
-}
type PolicyId
    = AdaPolicyId
    | CntPolicyId (Hash Blake2b_224)


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
    = VerificationKeyCredential (Hash Blake2b_224)
    | ScriptCredential (Hash Blake2b_224)


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
type Datum
    = NoDatum
    | DatumHash (Hash Blake2b_256)
    | InlineDatum Data


{-| An input eUTxO for a transaction.
-}
type alias Input =
    { outputReference : OutputReference
    , output : Output
    }


{-| The reference for a eUTxO.
-}
type alias OutputReference =
    { transactionId : { hash : Hash Blake2b_256 }
    , outputIndex : Int
    }


{-| The content of a eUTxO.
-}
type alias Output =
    { address : Address
    , value : Value
    , datum : Datum
    , referenceScript : Maybe (Hash Blake2b_224)
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
    = Mint PolicyId
    | Spend OutputReference
    | WithdrawFrom StakeCredential
    | Publish Certificate



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


toCbor : Transaction -> Bytes
toCbor tx =
    tx |> encodeTransaction |> E.encode


fromCbor : Bytes -> Maybe Transaction
fromCbor bytes =
    D.decode decodeTransaction bytes


encodeTransaction : Transaction -> E.Encoder
encodeTransaction _ =
    todo "encode tx"


decodeTransaction : D.Decoder Transaction
decodeTransaction =
    todo "decode tx"
