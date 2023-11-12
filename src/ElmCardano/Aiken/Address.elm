module ElmCardano.Aiken.Address exposing
    ( Address, Credential(..), StakeCredential(..), CredentialHash
    , enterprise, script
    , toCbor
    )

{-| Handling Cardano addresses.

@docs Address, Credential, StakeCredential, CredentialHash

@docs enterprise, script

@docs toCbor

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cbor.Encode as E


{-| An on-chain address with a structure matching Aiken `Address`.

Beware that they are to be encoded differently depending on the context.
In the off-chain context, within our [Address] type, the CBOR encoding for the whole address (with network ID),
must follow CIP-0019::ShelleyAddresses specification with the header and credentials.
So in the off-chain context, it makes no sense to encode [OnChainAddress].
One should directly encode [Address] instead to have the network ID.

In the on-chain (Plutus) context, [OnChainAddress] can be encoded to a structure equivalent to Aiken `Address`.
This is convenient to be able to use these addresses in datums and redeemers.

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
    = VerificationKeyCredential (Bytes CredentialHash)
    | ScriptCredential (Bytes CredentialHash)


{-| A StakeCredential represents the delegation and rewards withdrawal conditions associated with some stake address / account.

A StakeCredential is either provided inline, or, by reference using an on-chain pointer.
Read more about pointers in CIP-0019 :: Pointers.

-}
type StakeCredential
    = InlineCredential Credential
    | PointerCredential { slotNumber : Int, transactionIndex : Int, certificateIndex : Int }


{-| Phantom type for 28-bytes credential hashes.
This is a Blake2b-224 hash.
-}
type CredentialHash
    = CredentialHash Never


{-| Create a simple enterprise address, with only a payment credential and no stake credential.
-}
enterprise : Bytes CredentialHash -> Address
enterprise credentials =
    { paymentCredential = VerificationKeyCredential credentials
    , stakeCredential = Nothing
    }


{-| Create a simple script address, with only a payment credential and no stake credential.
-}
script : Bytes CredentialHash -> Address
script credentials =
    { paymentCredential = ScriptCredential credentials
    , stakeCredential = Nothing
    }


{-| Encode to CBOR an on-chain address for use in a datum or redeemer (matching Aiken `Address`).
-}
toCbor : Address -> E.Encoder
toCbor _ =
    Debug.todo "toCbor"
