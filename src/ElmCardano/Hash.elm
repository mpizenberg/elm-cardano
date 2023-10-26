module ElmCardano.Hash exposing
    ( Hash, Blake2b_224, Blake2b_256
    , blake2b_224, blake2b_256
    , asBytes, asHex
    )

{-| This module defines Hash, a custom type for readability, internally just holding bytes.

On-chain, any hash digest value is represented as a plain ‘ByteArray’.
Though in practice, hashes come from different sources and have different semantics.
The Hash type helps writing functions signatures with more meaningful types than mere ‘ByteArray’.

@docs Hash, Blake2b_224, Blake2b_256
@docs blake2b_224, blake2b_256
@docs asBytes, asHex

-}

import Bytes exposing (Bytes)
import ComparableBytes exposing (ComparableBytes)


type Hash a
    = Hash ComparableBytes


{-| The Blake2b-224 hash algorithm, typically used for credentials and policy IDs.
-}
type Blake2b_224
    = Blake2b_224 Never


{-| The Blake2b-256 hash algorithm, typically used for transaction IDs.
-}
type Blake2b_256
    = Blake2b_256 Never


blake2b_224 : String -> Hash Blake2b_224
blake2b_224 hexStr =
    -- TODO: add some garanties here about that hex
    Hash (ComparableBytes.fromStringUnchecked hexStr)


blake2b_256 : String -> Hash Blake2b_256
blake2b_256 hexStr =
    -- TODO: add some garanties here about that hex
    Hash (ComparableBytes.fromStringUnchecked hexStr)


asBytes : Hash a -> Bytes
asBytes (Hash bytes) =
    ComparableBytes.asBytes bytes


asHex : Hash a -> String
asHex (Hash bytes) =
    ComparableBytes.asHex bytes
