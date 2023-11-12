module ElmCardano.Address exposing
    ( Address, NetworkId(..)
    , OnChainAddress, Credential(..), StakeCredential(..), CredentialHash
    , enterprise, script
    , toCbor, encodeNetworkId
    )

{-| Handling Cardano addresses.

@docs Address, NetworkId

@docs OnChainAddress, Credential, StakeCredential, CredentialHash

@docs enterprise, script

@docs toCbor, encodeNetworkId

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cbor.Encode as E


{-| Full address, including the network ID.
-}
type alias Address =
    { networkId : NetworkId
    , onChainAddress : OnChainAddress
    }


{-| The network ID of a transaction.
-}
type NetworkId
    = Testnet -- 0
    | Mainnet -- 1


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
type alias OnChainAddress =
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
enterprise : Bytes CredentialHash -> OnChainAddress
enterprise credentials =
    { paymentCredential = VerificationKeyCredential credentials
    , stakeCredential = Nothing
    }


{-| Create a simple script address, with only a payment credential and no stake credential.
-}
script : Bytes CredentialHash -> OnChainAddress
script credentials =
    { paymentCredential = ScriptCredential credentials
    , stakeCredential = Nothing
    }


{-| Encode an [Address] to CBOR.

Shelley address description from CIP-0019:

    Header type (tttt....)  Payment Part     Delegation Part
    (0) 0000....            PaymentKeyHash   StakeKeyHash
    (1) 0001....            ScriptHash       StakeKeyHash
    (2) 0010....            PaymentKeyHash   ScriptHash
    (3) 0011....            ScriptHash       ScriptHash
    (4) 0100....            PaymentKeyHash   Pointer
    (5) 0101....            ScriptHash       Pointer
    (6) 0110....            PaymentKeyHash   ø
    (7) 0111....            ScriptHash       ø

    Header type (....tttt)
    (0) ....0000 testnet
    (1) ....0001 mainnet

For example, `61....(56 chars / 28 bytes)....` is an enterprise address (6, only a payment key) on mainnet (1).

Warning: do not use [onChainAddressToCbor] encoder inside this encoder.
The on-chain encoder is to match Aiken on-chain addresses.

-}
toCbor : Address -> E.Encoder
toCbor { networkId, onChainAddress } =
    let
        network =
            case networkId of
                Testnet ->
                    "0"

                Mainnet ->
                    "1"

        encodeAddress headerType payment stake =
            (headerType ++ network ++ payment ++ stake)
                |> Bytes.fromStringUnchecked
                |> Bytes.toCbor
    in
    case ( onChainAddress.paymentCredential, onChainAddress.stakeCredential ) of
        -- (0) 0000.... PaymentKeyHash StakeKeyHash
        ( VerificationKeyCredential paymentKeyHash, Just (InlineCredential (VerificationKeyCredential stakeKeyHash)) ) ->
            encodeAddress "0" (Bytes.toString paymentKeyHash) (Bytes.toString stakeKeyHash)

        -- (1) 0001.... ScriptHash StakeKeyHash
        ( ScriptCredential paymentScriptHash, Just (InlineCredential (VerificationKeyCredential stakeKeyHash)) ) ->
            encodeAddress "1" (Bytes.toString paymentScriptHash) (Bytes.toString stakeKeyHash)

        -- (2) 0010.... PaymentKeyHash ScriptHash
        ( VerificationKeyCredential paymentKeyHash, Just (InlineCredential (ScriptCredential stakeScriptHash)) ) ->
            encodeAddress "2" (Bytes.toString paymentKeyHash) (Bytes.toString stakeScriptHash)

        -- (3) 0011.... ScriptHash ScriptHash
        ( ScriptCredential paymentScriptHash, Just (InlineCredential (ScriptCredential stakeScriptHash)) ) ->
            encodeAddress "3" (Bytes.toString paymentScriptHash) (Bytes.toString stakeScriptHash)

        -- (4) 0100.... PaymentKeyHash Pointer
        ( VerificationKeyCredential paymentKeyHash, Just (PointerCredential { slotNumber, transactionIndex, certificateIndex }) ) ->
            encodeAddress "4" (Bytes.toString paymentKeyHash) (Debug.todo "encode pointer credential")

        -- (5) 0101.... ScriptHash Pointer
        ( ScriptCredential paymentScriptHash, Just (PointerCredential { slotNumber, transactionIndex, certificateIndex }) ) ->
            encodeAddress "5" (Bytes.toString paymentScriptHash) (Debug.todo "encode pointer credential")

        -- (6) 0110.... PaymentKeyHash ø
        ( VerificationKeyCredential paymentKeyHash, Nothing ) ->
            encodeAddress "6" (Bytes.toString paymentKeyHash) ""

        -- (7) 0111.... ScriptHash ø
        ( ScriptCredential paymentScriptHash, Nothing ) ->
            encodeAddress "7" (Bytes.toString paymentScriptHash) ""


{-| CBOR encoder for [NetworkId].
-}
encodeNetworkId : NetworkId -> E.Encoder
encodeNetworkId networkId =
    E.int <|
        case networkId of
            Testnet ->
                0

            Mainnet ->
                1


{-| Encode to CBOR an on-chain address for use in a datum or redeemer (matching Aiken `Address`).
-}
onChainAddressToCbor : OnChainAddress -> E.Encoder
onChainAddressToCbor onChainAddress =
    Debug.todo "onChainAddressToCbor"
