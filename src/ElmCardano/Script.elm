module ElmCardano.Script exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import ElmCardano.Hash exposing (Blake2b_224)


{-| <https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/babbage/model.rs#L58>
-}



-- script = [ 0, native_script // 1, plutus_v1_script // 2, plutus_v2_script ]


type Script
    = Native NativeScript
    | PlutusV1 PlutusV1Script
    | PlutusV2 PlutusV2Script


{-| A native script
<https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/alonzo/model.rs#L772>
-}
type NativeScript
    = ScriptPubkey Blake2b_224
    | ScriptAll (List NativeScript)
    | ScriptAny (List NativeScript)
    | ScriptNofK Int (List NativeScript)
    | InvalidBefore Int
    | InvalidHereafter Int


type alias PlutusScript =
    Bytes


type alias PlutusV1Script =
    Bytes


type alias PlutusV2Script =
    Bytes
