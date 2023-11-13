module ElmCardano.Address exposing
    ( Address(..), StakeAddress, NetworkId(..), ByronAddress
    , Credential(..), StakeCredential(..), CredentialHash
    , enterprise, script
    , toCbor, stakeAddressToCbor, credentialToCbor, encodeNetworkId
    )

{-| Handling Cardano addresses.

@docs Address, StakeAddress, NetworkId, ByronAddress

@docs Credential, StakeCredential, CredentialHash

@docs enterprise, script

@docs toCbor, stakeAddressToCbor, credentialToCbor, encodeNetworkId

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cbor.Encode as E
import Cbor.Encode.Extra as EE


{-| Full address, including the network ID.
-}
type Address
    = Byron (Bytes ByronAddress)
    | Shelley { networkId : NetworkId, paymentCredential : Credential, stakeCredential : Maybe StakeCredential }


{-| An address type only use for things related to staking, such as delegation and reward withdrawals.
-}
type alias StakeAddress =
    { networkId : NetworkId
    , stakeCredential : Credential
    }


{-| The network ID of a transaction.
-}
type NetworkId
    = Testnet -- 0
    | Mainnet -- 1


{-| Phantom type for Byron addresses.
-}
type ByronAddress
    = ByronAddress Never


{-| A general structure for representing an on-chain credential.

[Credential] can represent both payment credentials or stake credentials.

Credentials are always one of two kinds: a direct public/private key pair, or a script (native or Plutus).

-}
type Credential
    = VKeyHash (Bytes CredentialHash)
    | ScriptHash (Bytes CredentialHash)


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
enterprise : NetworkId -> Bytes CredentialHash -> Address
enterprise networkId credentials =
    Shelley
        { networkId = networkId
        , paymentCredential = VKeyHash credentials
        , stakeCredential = Nothing
        }


{-| Create a simple script address, with only a payment credential and no stake credential.
-}
script : NetworkId -> Bytes CredentialHash -> Address
script networkId credentials =
    Shelley
        { networkId = networkId
        , paymentCredential = ScriptHash credentials
        , stakeCredential = Nothing
        }


{-| Encode an [Address] to CBOR.

Byron addresses are left untouched as we don't plan to have full support of Byron era.

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

Stake address description from CIP-0019:

    Header type (tttt....)  Stake Reference
    (14) 1110....           StakeKeyHash
    (15) 1111....           ScriptHash

-}
toCbor : Address -> E.Encoder
toCbor address =
    case address of
        Byron bytes ->
            Bytes.toCbor bytes

        Shelley { networkId, paymentCredential, stakeCredential } ->
            case ( paymentCredential, stakeCredential ) of
                -- (0) 0000.... PaymentKeyHash StakeKeyHash
                ( VKeyHash paymentKeyHash, Just (InlineCredential (VKeyHash stakeKeyHash)) ) ->
                    encodeAddress networkId "0" (Bytes.toString paymentKeyHash ++ Bytes.toString stakeKeyHash)

                -- (1) 0001.... ScriptHash StakeKeyHash
                ( ScriptHash paymentScriptHash, Just (InlineCredential (VKeyHash stakeKeyHash)) ) ->
                    encodeAddress networkId "1" (Bytes.toString paymentScriptHash ++ Bytes.toString stakeKeyHash)

                -- (2) 0010.... PaymentKeyHash ScriptHash
                ( VKeyHash paymentKeyHash, Just (InlineCredential (ScriptHash stakeScriptHash)) ) ->
                    encodeAddress networkId "2" (Bytes.toString paymentKeyHash ++ Bytes.toString stakeScriptHash)

                -- (3) 0011.... ScriptHash ScriptHash
                ( ScriptHash paymentScriptHash, Just (InlineCredential (ScriptHash stakeScriptHash)) ) ->
                    encodeAddress networkId "3" (Bytes.toString paymentScriptHash ++ Bytes.toString stakeScriptHash)

                -- (4) 0100.... PaymentKeyHash Pointer
                ( VKeyHash paymentKeyHash, Just (PointerCredential _) ) ->
                    encodeAddress networkId "4" (Bytes.toString paymentKeyHash ++ Debug.todo "encode pointer credential")

                -- (5) 0101.... ScriptHash Pointer
                ( ScriptHash paymentScriptHash, Just (PointerCredential _) ) ->
                    encodeAddress networkId "5" (Bytes.toString paymentScriptHash ++ Debug.todo "encode pointer credential")

                -- (6) 0110.... PaymentKeyHash ø
                ( VKeyHash paymentKeyHash, Nothing ) ->
                    encodeAddress networkId "6" (Bytes.toString paymentKeyHash)

                -- (7) 0111.... ScriptHash ø
                ( ScriptHash paymentScriptHash, Nothing ) ->
                    encodeAddress networkId "7" (Bytes.toString paymentScriptHash)


{-| CBOR encoder for a stake address.
-}
stakeAddressToCbor : StakeAddress -> E.Encoder
stakeAddressToCbor { networkId, stakeCredential } =
    case stakeCredential of
        -- (14) 1110.... StakeKeyHash
        VKeyHash stakeKeyHash ->
            encodeAddress networkId "e" (Bytes.toString stakeKeyHash)

        -- (15) 1111.... ScriptHash
        ScriptHash stakeScriptHash ->
            encodeAddress networkId "f" (Bytes.toString stakeScriptHash)


encodeAddress : NetworkId -> String -> String -> E.Encoder
encodeAddress networkId headerType payload =
    let
        network =
            case networkId of
                Testnet ->
                    "0"

                Mainnet ->
                    "1"
    in
    (headerType ++ network ++ payload)
        |> Bytes.fromStringUnchecked
        |> Bytes.toCbor


{-| CBOR encoder for a [Credential], be it for payment or for stake.
-}
credentialToCbor : Credential -> E.Encoder
credentialToCbor stakeCredential =
    EE.ledgerList identity <|
        case stakeCredential of
            VKeyHash addrKeyHash ->
                [ E.int 0
                , Bytes.toCbor addrKeyHash
                ]

            ScriptHash scriptHash ->
                [ E.int 1
                , Bytes.toCbor scriptHash
                ]


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
