module Cardano.ScriptTests exposing (..)

import Cardano.Script as Script exposing (NativeScript, PlutusScript, Script)
import Cbor.Test exposing (roundtrip)
import Fuzz exposing (Fuzzer)
import Fuzz.Extra as Fuzz
import Test exposing (Test, describe)


suite : Test
suite =
    describe "Script"
        [ describe "toCbor >> fromCbor"
            [ roundtrip Script.toCbor Script.fromCbor fuzzer
            ]
        ]


fuzzer : Fuzzer Script
fuzzer =
    Fuzz.oneOf
        [ Fuzz.map Script.Native nativeScriptFuzzer
        , Fuzz.map Script.Plutus plutusScriptFuzzer
        ]


nativeScriptFuzzer : Fuzzer NativeScript
nativeScriptFuzzer =
    Fuzz.lazy
        (\_ ->
            Fuzz.frequency
                [ ( 50, Fuzz.map Script.ScriptPubkey (Fuzz.bytesOfSize 28) )
                , ( 10, Fuzz.map Script.ScriptAll (Fuzz.listOfLengthBetween 0 5 nativeScriptFuzzer) )
                , ( 10, Fuzz.map Script.ScriptAny (Fuzz.listOfLengthBetween 0 5 nativeScriptFuzzer) )
                , ( 10, Fuzz.map2 Script.ScriptNofK (Fuzz.intAtMost 5) (Fuzz.listOfLengthBetween 0 5 nativeScriptFuzzer) )
                , ( 10, Fuzz.map Script.InvalidBefore Fuzz.natural )
                , ( 10, Fuzz.map Script.InvalidHereafter Fuzz.natural )
                ]
        )


plutusScriptFuzzer : Fuzzer PlutusScript
plutusScriptFuzzer =
    Fuzz.map2 PlutusScript
        plutusVersionFuzzer
        Fuzz.bytes


plutusVersionFuzzer : Fuzzer Script.PlutusVersion
plutusVersionFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Script.PlutusV1
        , Fuzz.constant Script.PlutusV2
        , Fuzz.constant Script.PlutusV3
        ]
