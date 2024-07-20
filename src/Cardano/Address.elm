module Cardano.Address exposing
    ( Address(..), StakeAddress, NetworkId(..), ByronAddress
    , Credential(..), StakeCredential(..), StakeCredentialPointer, CredentialHash
    , enterprise, script, base, pointer
    , toCbor, stakeAddressToCbor, credentialToCbor, encodeNetworkId
    , decode, decodeReward
    )

{-| Handling Cardano addresses.

@docs Address, StakeAddress, NetworkId, ByronAddress

@docs Credential, StakeCredential, StakeCredentialPointer, CredentialHash

@docs enterprise, script, base, pointer

@docs toCbor, stakeAddressToCbor, credentialToCbor, encodeNetworkId

@docs decode, decodeReward

-}

import Bitwise
import Bytes as B
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Decode as BD
import Cbor.Decode as D
import Cbor.Encode as E
import Cbor.Encode.Extra as EE
import Word7


{-| Full address, including the network ID.
-}
type Address
    = Byron (Bytes ByronAddress)
    | Shelley { networkId : NetworkId, paymentCredential : Credential, stakeCredential : Maybe StakeCredential }
    | Reward StakeAddress


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
    | PointerCredential StakeCredentialPointer


{-| A stake credential pointer.

This should not be used and is only present for compatibility with previous eras.

-}
type alias StakeCredentialPointer =
    { slotNumber : Int, transactionIndex : Int, certificateIndex : Int }


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


{-| Create a base address with a payement credential and a stake credential.
-}
base : NetworkId -> Credential -> Credential -> Address
base networkId paymentCredential inlineStakeCredential =
    Shelley
        { networkId = networkId
        , paymentCredential = paymentCredential
        , stakeCredential = Just <| InlineCredential inlineStakeCredential
        }


{-| Create a pointer address.
-}
pointer : NetworkId -> Credential -> { slotNumber : Int, transactionIndex : Int, certificateIndex : Int } -> Address
pointer networkId paymentCredential p =
    Shelley
        { networkId = networkId
        , paymentCredential = paymentCredential
        , stakeCredential = Just <| PointerCredential p
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

        Reward stakeAddress ->
            stakeAddressToCbor stakeAddress


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



-- Decode


{-| CBOR decoder for [Address].
-}
decode : D.Decoder Address
decode =
    D.bytes
        |> D.andThen
            (\bytes ->
                case BD.decode (decodeBytes bytes) bytes of
                    Just address ->
                        D.succeed address

                    Nothing ->
                        let
                            _ =
                                Debug.log "Failed to decode address" (Bytes.toString <| Bytes.fromBytes bytes)
                        in
                        D.fail
            )


{-| CBOR decoder for [StakeAddress].
This only succeeds for a valid [Address] of the [Reward] variant.
-}
decodeReward : D.Decoder StakeAddress
decodeReward =
    decode
        |> D.andThen
            (\addr ->
                case addr of
                    Reward stakeAddress ->
                        D.succeed stakeAddress

                    _ ->
                        D.fail
            )


{-| Address decoder from raw bytes. Internal use only.
Cannot be compose with other decoders as it may not consume the correct number of bytes.

This needs a copy of the bytes of the address to compensate
the absence of backtracking in bytes decoders.
So if we read the first byte and

-}
decodeBytes : B.Bytes -> BD.Decoder Address
decodeBytes bytesCopy =
    BD.unsignedInt8
        |> BD.andThen
            (\header ->
                case Bitwise.shiftRightBy 4 header of
                    -- (0) 0000.... PaymentKeyHash StakeKeyHash
                    0 ->
                        BD.map2
                            (\payment stake -> base (networkIdFromHeader header) (VKeyHash payment) (VKeyHash stake))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (1) 0001.... ScriptHash StakeKeyHash
                    1 ->
                        BD.map2
                            (\payment stake -> base (networkIdFromHeader header) (ScriptHash payment) (VKeyHash stake))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (2) 0010.... PaymentKeyHash ScriptHash
                    2 ->
                        BD.map2
                            (\payment stake -> base (networkIdFromHeader header) (VKeyHash payment) (ScriptHash stake))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (3) 0011.... ScriptHash ScriptHash
                    3 ->
                        BD.map2
                            (\payment stake -> base (networkIdFromHeader header) (ScriptHash payment) (ScriptHash stake))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (4) 0100.... PaymentKeyHash Pointer
                    4 ->
                        BD.map2
                            (\payment ->
                                pointer (networkIdFromHeader header) (VKeyHash payment)
                            )
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map3 StakeCredentialPointer Word7.fromBytes Word7.fromBytes Word7.fromBytes)

                    -- (5) 0101.... ScriptHash Pointer
                    5 ->
                        BD.map2
                            (\payment ->
                                pointer (networkIdFromHeader header) (ScriptHash payment)
                            )
                            (BD.map Bytes.fromBytes <| BD.bytes 28)
                            (BD.map3 StakeCredentialPointer Word7.fromBytes Word7.fromBytes Word7.fromBytes)

                    -- (6) 0110.... PaymentKeyHash ø
                    6 ->
                        BD.map (enterprise (networkIdFromHeader header))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (7) 0111.... ScriptHash ø
                    7 ->
                        BD.map (script (networkIdFromHeader header))
                            (BD.map Bytes.fromBytes <| BD.bytes 28)

                    -- (8) 1000.... Byron
                    8 ->
                        BD.map (always <| Byron (Bytes.fromBytes bytesCopy)) <|
                            BD.bytes (B.width bytesCopy - 1)

                    -- (14) 1110.... StakeKeyHash
                    14 ->
                        BD.map (\cred -> Reward <| StakeAddress (networkIdFromHeader header) cred)
                            (BD.map (VKeyHash << Bytes.fromBytes) <| BD.bytes 28)

                    -- (15) 1111.... ScriptHash
                    15 ->
                        BD.map (\cred -> Reward <| StakeAddress (networkIdFromHeader header) cred)
                            (BD.map (ScriptHash << Bytes.fromBytes) <| BD.bytes 28)

                    _ ->
                        BD.fail
            )


networkIdFromHeader : Int -> NetworkId
networkIdFromHeader header =
    case Bitwise.and 0x0F header of
        0 ->
            Testnet

        1 ->
            Mainnet

        n ->
            Debug.todo ("Unrecognized network id:" ++ String.fromInt n)
