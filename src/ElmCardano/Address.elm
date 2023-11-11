module ElmCardano.Address exposing (Address, Credential(..), StakeCredential(..), CredentialHash)

{-| Handling Cardano addresses.

@docs Address, Credential, StakeCredential, CredentialHash

-}

import Bytes.Comparable exposing (Bytes)


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
