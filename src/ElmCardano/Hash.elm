module ElmCardano.Hash exposing (Hash, Blake2b_224, Blake2b_256)

{-| This module defines Hash, a custom type for readability, internally just holding bytes.

On-chain, any hash digest value is represented as a plain ‘ByteArray’.
Though in practice, hashes come from different sources and have different semantics.
The Hash type helps writing functions signatures with more meaningful types than mere ‘ByteArray’.

@docs Hash, Blake2b_224, Blake2b_256

-}

import Array exposing (Array)


{-| The Hash type, with two phantom types for more semantic meaning.
The `algo` type identifies the hashing algorithm.
Internally, this is just a byte array.
-}
type Hash algo
    = Hash (Array Int)


{-| The Blake2b-224 hash algorithm, typically used for credentials and policy IDs.
-}
type Blake2b_224
    = Blake2b_224


{-| The Blake2b-256 hash algorithm, typically used for transaction IDs.
-}
type Blake2b_256
    = Blake2b_256
