module ElmCardano.Hash exposing (Blake2b_224, Blake2b_256)

{-| This module defines Hash, a custom type for readability, internally just holding bytes.

On-chain, any hash digest value is represented as a plain ‘ByteArray’.
Though in practice, hashes come from different sources and have different semantics.
The Hash type helps writing functions signatures with more meaningful types than mere ‘ByteArray’.

@docs Hash, Blake2b_224, Blake2b_256

-}

import Bytes exposing (Bytes)


{-| The Blake2b-224 hash algorithm, typically used for credentials and policy IDs.
-}
type alias Blake2b_224 =
    Bytes


{-| The Blake2b-256 hash algorithm, typically used for transaction IDs.
-}
type alias Blake2b_256 =
    Bytes
