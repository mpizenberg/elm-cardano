module ElmCardano.Script exposing (Script(..), NativeScript(..), PlutusScript)

{-| Script

@docs Script, NativeScript, PlutusScript

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import ElmCardano.Hash exposing (Blake2b_224, Hash)


{-| <https://github.com/txpipe/pallas/blob/d1ac0561427a1d6d1da05f7b4ea21414f139201e/pallas-primitives/src/babbage/model.rs#L58>
-}



-- script = [ 0, native_script // 1, plutus_v1_script // 2, plutus_v2_script ]


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


type alias PlutusScript =
    { version : Int
    , script : Bytes
    }
