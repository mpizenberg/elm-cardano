module Cardano.Uplc exposing (..)

import Bytes.Comparable as Bytes
import Cardano.Redeemer exposing (ExUnits, Redeemer)
import Cardano.Transaction as Transaction exposing (CostModels, Transaction)
import Cardano.Utxo as Utxo exposing (Output)
import Cbor.Encode
import Dict
import Dict.Any
import Json.Encode as JE
import Natural exposing (Natural)


{-| Evaluate plutus scripts costs.
-}
evalScriptsCosts : VmConfig -> Utxo.RefDict Output -> Transaction -> Result String (List Redeemer)
evalScriptsCosts vmConfig localStateUtxos tx =
    let
        jsEncode : (a -> Cbor.Encode.Encoder) -> a -> JE.Value
        jsEncode cborEncoder v =
            Cbor.Encode.encode (cborEncoder v)
                |> Bytes.fromBytes
                |> Bytes.toString
                |> JE.string

        usedUtxos : Utxo.RefDict (Maybe Output)
        usedUtxos =
            Transaction.allInputs tx
                |> Dict.Any.foldl
                    (\ref _ acc ->
                        Dict.Any.insert ref (Dict.Any.get ref localStateUtxos) acc
                    )
                    Utxo.emptyRefDict

        missingUtxos =
            Dict.Any.filter (\_ o -> o == Nothing) usedUtxos
                |> Dict.Any.keys
    in
    if not (List.isEmpty missingUtxos) then
        Err "Missing UTxOs in local state: (TODO details)"

    else
        let
            ( refs, outputs ) =
                Dict.Any.filterMap (\_ -> identity) usedUtxos
                    |> (\utxos -> ( Dict.Any.keys utxos, Dict.Any.values utxos ))

            jsArguments =
                JE.object
                    [ ( "tx_bytes", JE.string <| Bytes.toString <| Transaction.serialize tx )
                    , ( "utxos_refs_bytes", JE.list (jsEncode Utxo.encodeOutputReference) refs )
                    , ( "utxos_outputs_bytes", JE.list (jsEncode Utxo.encodeOutput) outputs )
                    ]
        in
        evalScriptsCostsKernel jsArguments


{-| Kernel function (needs patching by elm-cardano) to run phase 2 evaluation (WASM code).
-}
evalScriptsCostsKernel : JE.Value -> Result String (List Redeemer)
evalScriptsCostsKernel _ =
    Err "evalScriptsCostsKernel"


type alias VmConfig =
    { budget : ExUnits
    , slotConfig : SlotConfig
    , costModels : CostModels
    }


conwayDefaultBudget : ExUnits
conwayDefaultBudget =
    { mem = 14000000, steps = 10000000000 }


type alias SlotConfig =
    { zeroTime : Natural
    , zeroSlot : Natural
    , slotLengthMs : Int
    }


slotConfigMainnet : SlotConfig
slotConfigMainnet =
    -- Found in Blaze codebase
    { zeroTime = Natural.fromSafeInt 1596059091000
    , zeroSlot = Natural.fromSafeInt 4492800
    , slotLengthMs = 1000
    }


slotConfigPreview : SlotConfig
slotConfigPreview =
    -- Found in Blaze codebase
    { zeroTime = Natural.fromSafeInt 1666656000000
    , zeroSlot = Natural.fromSafeInt 0
    , slotLengthMs = 1000
    }


slotConfigPreprod : SlotConfig
slotConfigPreprod =
    -- Found in Blaze codebase
    { zeroTime = Natural.fromSafeInt (1654041600000 + 1728000000)
    , zeroSlot = Natural.fromSafeInt 86400
    , slotLengthMs = 1000
    }
