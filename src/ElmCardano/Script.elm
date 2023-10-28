module ElmCardano.Script exposing (Script(..), NativeScript(..), PlutusScript)

{-| Script

@docs Script, NativeScript, PlutusScript

-}

import Bytes.Comparable exposing (Bytes)
import ElmCardano.Hash exposing (Blake2b_224, Hash)


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
    { version : Int
    , script : Bytes
    }
