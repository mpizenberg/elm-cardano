module Blake2b.DigestTests exposing (suite)

import Blake2b exposing (blake2b512)
import Bytes.Comparable as CB
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Blake2b512" <|
        [ test "Digest of \"abc\"" <|
            \_ -> Expect.equal sampleDigest0 correctDigest0
        , test "Digest of an empty string" <|
            \_ -> Expect.equal sampleDigest1 correctDigest1
        , test "Digest of \"The quick brown fox jumps over the lazy dog\"" <|
            \_ -> Expect.equal sampleDigest2 correctDigest2
        , test "Digest of \"The quick brown fox jumps over the lazy dof\"" <|
            \_ -> Expect.equal sampleDigest3 correctDigest3
        ]


sampleText0 : String
sampleText0 =
    "abc"


sampleText1 : String
sampleText1 =
    "The quick brown fox jumps over the lazy dog"


sampleText2 : String
sampleText2 =
    "The quick brown fox jumps over the lazy dof"


sampleDigest0 : List Int
sampleDigest0 =
    CB.fromText sampleText0 |> CB.toU8 |> blake2b512 Nothing


sampleDigest1 : List Int
sampleDigest1 =
    blake2b512 Nothing []


sampleDigest2 : List Int
sampleDigest2 =
    CB.fromText sampleText1 |> CB.toU8 |> blake2b512 Nothing


sampleDigest3 : List Int
sampleDigest3 =
    CB.fromText sampleText2 |> CB.toU8 |> blake2b512 Nothing


correctDigest0 : List Int
correctDigest0 =
    CB.fromString
        "ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923"
        |> Maybe.andThen (CB.toU8 >> Just)
        |> Maybe.withDefault []


correctDigest1 : List Int
correctDigest1 =
    CB.fromString
        "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"
        |> Maybe.andThen (CB.toU8 >> Just)
        |> Maybe.withDefault []


correctDigest2 : List Int
correctDigest2 =
    CB.fromString
        "a8add4bdddfd93e4877d2746e62817b116364a1fa7bc148d95090bc7333b3673f82401cf7aa2e4cb1ecd90296e3f14cb5413f8ed77be73045b13914cdcd6a918"
        |> Maybe.andThen (CB.toU8 >> Just)
        |> Maybe.withDefault []


correctDigest3 : List Int
correctDigest3 =
    CB.fromString
        "ab6b007747d8068c02e25a6008db8a77c218d94f3b40d2291a7dc8a62090a744c082ea27af01521a102e42f480a31e9844053f456b4b41e8aa78bbe5c12957bb"
        |> Maybe.andThen (CB.toU8 >> Just)
        |> Maybe.withDefault []
