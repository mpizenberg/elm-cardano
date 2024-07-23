module Cardano.Script exposing
    ( Script(..), NativeScript(..), NativeScriptPubkeyHash, PlutusScript, PlutusVersion(..), ScriptCbor
    , encodeScript, encodeNativeScript, encodePlutusScript
    , decodeNativeScript
    )

{-| Script

@docs Script, NativeScript, NativeScriptPubkeyHash, PlutusScript, PlutusVersion, ScriptCbor


## Encoders

@docs encodeScript, encodeNativeScript, encodePlutusScript


## Decoders

@docs decodeNativeScript

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cbor.Decode as D
import Cbor.Decode.Extra as DE
import Cbor.Encode as E
import Cbor.Encode.Extra as EE


{-| Cardano script, either a native script or a plutus script.

`script = [ 0, native_script // 1, plutus_v1_script // 2, plutus_v2_script ]`

[Babbage implementation in Pallas][pallas].

[pallas]: https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/babbage/model.rs#L58

-}
type Script
    = Native NativeScript
    | Plutus PlutusScript


{-| A native script
<https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L772>
-}
type NativeScript
    = ScriptPubkey (Bytes NativeScriptPubkeyHash)
    | ScriptAll (List NativeScript)
    | ScriptAny (List NativeScript)
    | ScriptNofK Int (List NativeScript)
    | InvalidBefore Int
    | InvalidHereafter Int


{-| Phantom type for 28-bytes native script public key hash.
This is a Blake2b-224 hash.
-}
type NativeScriptPubkeyHash
    = NativeScriptPubkeyHash Never


{-| A plutus script.
-}
type alias PlutusScript =
    { version : PlutusVersion
    , script : Bytes ScriptCbor
    }


{-| The plutus version.
-}
type PlutusVersion
    = PlutusV1
    | PlutusV2


{-| Phantom type describing the kind of bytes within a [PlutusScript] object.
-}
type ScriptCbor
    = ScriptCbor Never


{-| Cbor Encoder for [Script]
-}
encodeScript : Script -> E.Encoder
encodeScript script =
    case script of
        Native nativeScript ->
            EE.ledgerList identity
                [ E.int 0
                , encodeNativeScript nativeScript
                ]

        Plutus plutusScript ->
            EE.ledgerList identity
                [ encodePlutusVersion plutusScript.version
                , encodePlutusScript plutusScript
                ]


{-| Cbor Encoder for [NativeScript]
-}
encodeNativeScript : NativeScript -> E.Encoder
encodeNativeScript nativeScript =
    EE.ledgerList identity <|
        case nativeScript of
            ScriptPubkey addrKeyHash ->
                [ E.int 0
                , Bytes.toCbor addrKeyHash
                ]

            ScriptAll nativeScripts ->
                [ E.int 1
                , EE.ledgerList encodeNativeScript nativeScripts
                ]

            ScriptAny nativeScripts ->
                [ E.int 2
                , EE.ledgerList encodeNativeScript nativeScripts
                ]

            ScriptNofK atLeast nativeScripts ->
                [ E.int 3
                , E.int atLeast
                , EE.ledgerList encodeNativeScript nativeScripts
                ]

            InvalidBefore start ->
                [ E.int 4
                , E.int start
                ]

            InvalidHereafter end ->
                [ E.int 5
                , E.int end
                ]


{-| Cbor Encoder for PlutusScript
-}
encodePlutusScript : PlutusScript -> E.Encoder
encodePlutusScript { script } =
    Bytes.toCbor script


encodePlutusVersion : PlutusVersion -> E.Encoder
encodePlutusVersion version =
    E.int <|
        case version of
            PlutusV1 ->
                1

            PlutusV2 ->
                2



-- Decoders


{-| Decode NativeScript from CBOR.
-}
decodeNativeScript : D.Decoder NativeScript
decodeNativeScript =
    DE.failWith "decode native script unimplemented"
