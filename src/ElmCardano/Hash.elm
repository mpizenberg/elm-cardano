module ElmCardano.Hash exposing
    ( Hash, Blake2b_224, Blake2b_256
    , blake2b_224, blake2b_256
    , toBytes, toString
    , encode
    )

{-| This module defines Hash, a custom type for readability, internally just holding bytes.

On-chain, any hash digest value is represented as a plain ‘ByteArray’.
Though in practice, hashes come from different sources and have different semantics.
The Hash type helps writing functions signatures with more meaningful types than mere ‘ByteArray’.

@docs Hash, Blake2b_224, Blake2b_256
@docs blake2b_224, blake2b_256
@docs toBytes, toString
@docs encode

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cbor.Encode as E


{-| Hash bytes.

The free type parameter is a phantom type
ensuring the bytes follow some constaints related to the hash algorithm.

-}
type Hash a
    = Hash Bytes


{-| The Blake2b-224 hash algorithm, typically used for credentials and policy IDs.
-}
type Blake2b_224
    = Blake2b_224 Never


{-| The Blake2b-256 hash algorithm, typically used for transaction IDs.
-}
type Blake2b_256
    = Blake2b_256 Never


{-| Constructor for Blake2B-224 algorithm.

Warning: currently, this does not compute the hash of the string,
it just encapsulates it and mark it as a Blake2B-224 hash.

TODO: perform some checks to ensure the string has the correct shape.

-}
blake2b_224 : String -> Hash Blake2b_224
blake2b_224 hexStr =
    -- TODO: add some garanties here about that hex
    Hash (Bytes.fromStringUnchecked hexStr)


{-| Constructor for Blake2B-256 algorithm.

Warning: currently, this does not compute the hash of the string,
it just encapsulates it and mark it as a Blake2B-256 hash.

TODO: perform some checks to ensure the string has the correct shape.

-}
blake2b_256 : String -> Hash Blake2b_256
blake2b_256 hexStr =
    -- TODO: add some garanties here about that hex
    Hash (Bytes.fromStringUnchecked hexStr)


{-| Extract the [Bytes] of the hash.
-}
toBytes : Hash a -> Bytes
toBytes (Hash bytes) =
    bytes


{-| Convert the hash into its hex-encoded string equivalent.
-}
toString : Hash a -> String
toString (Hash bytes) =
    Bytes.toString bytes


{-| CBOR encoder for a hash.
-}
encode : Hash a -> E.Encoder
encode (Hash bytes) =
    E.bytes (Bytes.toBytes bytes)
