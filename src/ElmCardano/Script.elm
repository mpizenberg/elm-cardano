module ElmCardano.Script exposing
    ( Script(..), NativeScript(..), PlutusScript, PlutusVersion(..)
    , encodeScript, encodeNativeScript, encodePlutusScript
    )

{-| Script

@docs Script, NativeScript, PlutusScript, PlutusVersion


## Encoders

@docs encodeScript, encodeNativeScript, encodePlutusScript

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cbor.Encode as E
import ElmCardano.Hash as Hash exposing (Blake2b_224, Hash)


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
    = ScriptPubkey (Hash Blake2b_224)
    | ScriptAll (List NativeScript)
    | ScriptAny (List NativeScript)
    | ScriptNofK Int (List NativeScript)
    | InvalidBefore Int
    | InvalidHereafter Int


{-| A plutus script.
-}
type alias PlutusScript =
    { version : PlutusVersion
    , script : Bytes
    }


{-| The plutus version.
-}
type PlutusVersion
    = PlutusV1
    | PlutusV2


{-| Cbor Encoder for [Script]
-}
encodeScript : Script -> E.Encoder
encodeScript script =
    case script of
        Native nativeScript ->
            E.list identity
                [ E.int 0
                , encodeNativeScript nativeScript
                ]

        Plutus plutusScript ->
            E.list identity
                [ encodePlutusVersion plutusScript.version
                , encodePlutusScript plutusScript
                ]


{-| Cbor Encoder for [NativeScript]
-}
encodeNativeScript : NativeScript -> E.Encoder
encodeNativeScript nativeScript =
    E.list identity <|
        case nativeScript of
            ScriptPubkey addrKeyHash ->
                [ E.int 0
                , Hash.encode addrKeyHash
                ]

            ScriptAll nativeScripts ->
                [ E.int 1
                , E.list encodeNativeScript nativeScripts
                ]

            ScriptAny nativeScripts ->
                [ E.int 2
                , E.list encodeNativeScript nativeScripts
                ]

            ScriptNofK atLeast nativeScripts ->
                [ E.int 3
                , E.int atLeast
                , E.list encodeNativeScript nativeScripts
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
