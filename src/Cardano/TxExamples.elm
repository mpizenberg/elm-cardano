module Cardano.TxExamples exposing
    ( example1, example2, example3
    , prettyTx
    )

{-| Just a module to make sure that examples compile when we change stuff.

@docs example1, example2, example3

@docs prettyTx

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as Map
import Cardano exposing (ScriptWitness(..), SpendSource(..), TxIntent(..), TxOtherInfo(..), WitnessSource(..), dummyBytes, finalize)
import Cardano.Address as Address exposing (Address(..), Credential(..), CredentialHash, NetworkId(..), StakeAddress, StakeCredential(..))
import Cardano.Data as Data
import Cardano.Metadatum as Metadatum
import Cardano.MultiAsset as MultiAsset
import Cardano.Script as Script exposing (PlutusScript, PlutusVersion(..))
import Cardano.Transaction exposing (Transaction)
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Cbor.Encode as E
import Dict.Any
import Integer
import Natural exposing (Natural)



-- EXAMPLES global state


ada =
    -- Asset amounts are typed with unbounded Natural numbers
    { one = Value.onlyLovelace (Natural.fromSafeString "1000000")
    , two = Value.onlyLovelace (Natural.fromSafeString "2000000")
    , four = Value.onlyLovelace (Natural.fromSafeString "4000000")
    , ten = Value.onlyLovelace (Natural.fromSafeString "10000000")
    }


exAddr =
    { me = makeWalletAddress "me"
    , you = makeWalletAddress "you"
    }


dog =
    { policyId = dummyCredentialHash "dog"
    , policyIdStr = "dog"
    , assetName = Bytes.fromText "yksoh"
    , assetNameStr = "yksoh"
    , scriptRef = makeRef "dogScriptRef" 0
    , refOutput =
        { address = makeAddress "dogScriptRefAddress"
        , amount = ada.two
        , datumOption = Nothing
        , referenceScript = Just <| Script.Native <| Script.ScriptAll [] -- dummy
        }
    }


cat =
    { policyId = dummyCredentialHash "cat"
    , policyIdStr = "cat"
    , assetName = Bytes.fromText "felix"
    , assetNameStr = "felix"
    , scriptRef = makeRef "catScriptRef" 0
    , refOutput =
        { address = makeAddress "catScriptRefAddress"
        , amount = ada.two
        , datumOption = Nothing
        , referenceScript = Just <| Script.Native <| Script.ScriptAll [] -- dummy
        }
    }


globalStateUtxos : Utxo.RefDict Output
globalStateUtxos =
    Utxo.refDictFromList
        [ makeAdaOutput 0 exAddr.me 2 --   2 ada at my address
        , makeAdaOutput 1 exAddr.me 10 -- 10 ada at my address
        , makeAdaOutput 2 exAddr.me 5 --   5 ada at my address
        , makeAsset 3 exAddr.me dog.policyIdStr dog.assetNameStr 2
        , makeAsset 4 exAddr.me cat.policyIdStr cat.assetNameStr 5
        , ( dog.scriptRef, dog.refOutput )
        , ( cat.scriptRef, cat.refOutput )
        ]



-- EXAMPLE 1: Simple transfer


example1 _ =
    [ Spend <| FromWallet exAddr.me ada.one
    , SendTo exAddr.you ada.one
    ]
        |> finalize globalStateUtxos [ TxMetadata { tag = Natural.fromSafeInt 14, metadata = Metadatum.Int (Integer.fromSafeInt 42) } ]



-- EXAMPLE 2: mint/burn with native script


example2 _ =
    -- minting 1 dog (amounts are of type Integer: unbounded positive or negative integers)
    [ MintBurn
        { policyId = dog.policyId
        , assets = Map.singleton dog.assetName Integer.one
        , scriptWitness = NativeWitness (WitnessReference dog.scriptRef)
        }
    , SendTo exAddr.me (Value.onlyToken dog.policyId dog.assetName Natural.one)

    -- burning 1 cat
    , Spend <| FromWallet exAddr.me (Value.onlyToken cat.policyId cat.assetName Natural.one)
    , MintBurn
        { policyId = cat.policyId
        , assets = Map.singleton cat.assetName Integer.negativeOne
        , scriptWitness = NativeWitness (WitnessReference cat.scriptRef)
        }
    ]
        |> finalize globalStateUtxos []



-- EXAMPLE 3: spend from a Plutus script
-- The input index is provided in the redeemer


utxoBeingSpent =
    makeRef "previouslySentToLock" 0


findSpendingUtxo inputs =
    case inputs of
        [] ->
            0

        ( id, ref ) :: next ->
            if ref == utxoBeingSpent then
                id

            else
                findSpendingUtxo next


example3 _ =
    let
        ( myKeyCred, myStakeCred ) =
            ( Address.extractPubKeyHash exAddr.me
                |> Maybe.withDefault (Bytes.fromText "should not fail")
            , Address.extractStakeCredential exAddr.me
            )

        -- Lock script made with Aiken
        lock =
            { script = PlutusScript PlutusV3 (Bytes.fromStringUnchecked "58b501010032323232323225333002323232323253330073370e900118041baa0011323232533300a3370e900018059baa00113322323300100100322533301100114a0264a66601e66e3cdd718098010020a5113300300300130130013758601c601e601e601e601e601e601e601e601e60186ea801cdd7180718061baa00116300d300e002300c001300937540022c6014601600460120026012004600e00260086ea8004526136565734aae7555cf2ab9f5742ae881")
            , scriptHash = Bytes.fromStringUnchecked "3ff0b1bb5815347c6f0c05328556d80c1f83ca47ac410d25ffb4a330"
            }

        -- Combining the script hash with our stake credential
        -- to keep the locked ada staked.
        lockScriptAddress =
            Address.Shelley
                { networkId = Mainnet
                , paymentCredential = ScriptHash lock.scriptHash
                , stakeCredential = myStakeCred
                }

        -- Build a redeemer that contains the index of the spent script input.
        redeemer inputsOutputs =
            List.indexedMap Tuple.pair inputsOutputs.spentInputs
                |> findSpendingUtxo
                |> (Data.Int << Integer.fromSafeInt)

        -- Helper function to create an output at the lock script address.
        -- It contains our key credential in the datum.
        makeLockedOutput adaAmount =
            { address = lockScriptAddress
            , amount = adaAmount
            , datumOption = Just (DatumValue (Data.Bytes <| Bytes.toAny myKeyCred))
            , referenceScript = Nothing
            }

        -- Add to local state utxos some previously sent 4 ada.
        localStateUtxos =
            globalStateUtxos
                |> Dict.Any.insert utxoBeingSpent (makeLockedOutput ada.four)
    in
    -- Collect 2 ada from the lock script
    [ Spend <|
        FromPlutusScript
            { spentInput = utxoBeingSpent
            , datumWitness = Nothing
            , plutusScriptWitness =
                { script = WitnessValue lock.script
                , redeemerData = redeemer
                , requiredSigners = [ myKeyCred ]
                }
            }
    , SendTo exAddr.me ada.two

    -- Return the other 2 ada to the lock script (there was 4 ada initially)
    , SendToOutput (makeLockedOutput ada.two)
    ]
        |> finalize localStateUtxos []



-- Helper functions to build stuff


dummyCredentialHash : String -> Bytes CredentialHash
dummyCredentialHash str =
    dummyBytes 28 str


makeWalletAddress : String -> Address
makeWalletAddress name =
    Address.Shelley
        { networkId = Mainnet
        , paymentCredential = VKeyHash (dummyCredentialHash name)
        , stakeCredential = Just (InlineCredential (VKeyHash <| dummyCredentialHash name))
        }


makeAddress : String -> Address
makeAddress name =
    Address.enterprise Mainnet (dummyCredentialHash name)


makeRef : String -> Int -> OutputReference
makeRef id index =
    { transactionId = dummyBytes 32 id
    , outputIndex = index
    }


makeAsset : Int -> Address -> String -> String -> Int -> ( OutputReference, Output )
makeAsset index address policyId name amount =
    ( makeRef (String.fromInt index) index
    , { address = address
      , amount = makeToken policyId name amount
      , datumOption = Nothing
      , referenceScript = Nothing
      }
    )


makeAdaOutput : Int -> Address -> Int -> ( OutputReference, Output )
makeAdaOutput index address amount =
    ( makeRef (String.fromInt index) index
    , Utxo.fromLovelace address (Natural.fromSafeInt <| 1000000 * amount)
    )


makeToken : String -> String -> Int -> Value
makeToken policyId name amount =
    Value.onlyToken (dummyCredentialHash policyId) (Bytes.fromText name) (Natural.fromSafeInt amount)



-- Helper function to display pretty stuff


prettyAddr address =
    case address of
        Byron b ->
            (Bytes.toText >> Maybe.withDefault "") b

        Shelley { paymentCredential, stakeCredential } ->
            [ Just "Addr:", Just (prettyCred paymentCredential), Maybe.map prettyStakeCred stakeCredential ]
                |> List.filterMap identity
                |> String.join " "

        Reward stakeAddr ->
            "StakeAddr:" ++ prettyCred stakeAddr.stakeCredential


prettyStakeCred stakeCred =
    case stakeCred of
        Address.InlineCredential cred ->
            "stake:" ++ prettyCred cred

        Address.PointerCredential _ ->
            "stake:PointerAddr"


prettyCred cred =
    case cred of
        Address.VKeyHash b ->
            "key:" ++ (Bytes.toText >> Maybe.withDefault "") b

        Address.ScriptHash b ->
            "script:" ++ (Bytes.toText >> Maybe.withDefault "") b


prettyWithdrawal : ( StakeAddress, Natural ) -> String
prettyWithdrawal ( { stakeCredential }, amount ) =
    "₳ " ++ Natural.toString amount ++ " @ stakeCred:" ++ prettyCred stakeCredential


prettyValue : Value -> List String
prettyValue { lovelace, assets } =
    if MultiAsset.isEmpty assets then
        [ "₳ " ++ Natural.toString lovelace ]

    else
        "with native assets:"
            :: ("   ₳ " ++ Natural.toString lovelace)
            :: List.map (indent 3) (prettyAssets Natural.toString assets)


prettyAssets toStr multiAsset =
    Map.toList multiAsset
        |> List.concatMap
            (\( policyId, assets ) ->
                Map.toList assets
                    |> List.map
                        (\( name, amount ) ->
                            String.join " "
                                [ (Bytes.toText >> Maybe.withDefault "") policyId
                                , (Bytes.toText >> Maybe.withDefault "") name
                                , toStr amount
                                ]
                        )
            )


prettyDatum datumOption =
    case datumOption of
        Utxo.DatumHash h ->
            "datumHash: " ++ Maybe.withDefault "" (Bytes.toText h)

        Utxo.DatumValue data ->
            "datum: " ++ prettyCbor Data.toCbor data


prettyCbor toCbor x =
    E.encode (toCbor x) |> Bytes.fromBytes |> Bytes.toString


prettyScript script =
    case script of
        Script.Native nativeScript ->
            "NativeScript: " ++ prettyCbor Script.encodeNativeScript nativeScript

        Script.Plutus plutusScript ->
            "PlutusScript: " ++ prettyCbor Script.encodePlutusScript plutusScript


prettyInput ref =
    String.join " "
        [ "TxId:" ++ (Bytes.toText >> Maybe.withDefault "") ref.transactionId
        , "#" ++ String.fromInt ref.outputIndex
        ]


prettyOutput : Output -> List String
prettyOutput { address, amount, datumOption, referenceScript } =
    ("- " ++ prettyAddr address)
        :: ([ Just <| prettyValue amount
            , Maybe.map (List.singleton << prettyDatum) datumOption
            , Maybe.map (List.singleton << prettyScript) referenceScript
            ]
                |> List.filterMap identity
                |> List.concat
                |> List.map (indent 2)
           )


prettyList sectionTitle prettify list =
    if List.isEmpty list then
        []

    else
        sectionTitle
            :: List.map (indent 3 << prettify) list


prettyMints sectionTitle multiAsset =
    if MultiAsset.isEmpty multiAsset then
        []

    else
        sectionTitle
            :: List.map (indent 3) (prettyAssets Integer.toString multiAsset)


prettyVKeyWitness { vkey, signature } =
    String.join ", "
        [ "vkey:" ++ (Bytes.toText vkey |> Maybe.withDefault "")
        , "signature:" ++ (Bytes.toText signature |> Maybe.withDefault "")
        ]


prettyRedeemer redeemer =
    String.join " "
        [ Debug.toString redeemer.tag
        , "index:" ++ String.fromInt redeemer.index
        , "exUnits: mem " ++ String.fromInt redeemer.exUnits.mem ++ ", steps " ++ String.fromInt redeemer.exUnits.steps
        , "data:" ++ prettyCbor Data.toCbor redeemer.data
        ]


prettyMetadata ( tag, metadatum ) =
    Natural.toString tag ++ ": " ++ prettyCbor Metadatum.toCbor metadatum


indent spaces str =
    String.repeat spaces " " ++ str


prettyTx : Transaction -> String
prettyTx tx =
    let
        prettyBytes b =
            Maybe.withDefault (Bytes.toString b) (Bytes.toText b)

        body =
            List.concat
                [ [ "Tx fee: ₳ " ++ (Maybe.withDefault Natural.zero tx.body.fee |> Natural.toString) ]
                , prettyList "Tx ref inputs:" prettyInput tx.body.referenceInputs
                , prettyList "Tx inputs:" prettyInput tx.body.inputs
                , [ "Tx outputs:" ]
                , List.concatMap prettyOutput tx.body.outputs
                    |> List.map (indent 3)
                , prettyMints "Tx mints:" tx.body.mint
                , prettyList "Tx withdrawals:" prettyWithdrawal tx.body.withdrawals
                , prettyList "Tx required signers:" prettyBytes tx.body.requiredSigners
                , prettyList
                    ("Tx collateral (total: ₳ " ++ (Maybe.withDefault "not set" <| Maybe.map String.fromInt tx.body.totalCollateral) ++ "):")
                    prettyInput
                    tx.body.collateral
                , Maybe.map prettyOutput tx.body.collateralReturn
                    |> Maybe.withDefault []
                    |> prettyList "Tx collateral return:" identity
                ]

        witnessSet =
            List.concat <|
                List.filterMap identity
                    [ tx.witnessSet.vkeywitness
                        |> Maybe.map (prettyList "Tx vkey witness:" prettyVKeyWitness)
                    , tx.witnessSet.nativeScripts
                        |> Maybe.map (prettyList "Tx native scripts:" (prettyScript << Script.Native))
                    , tx.witnessSet.plutusV1Script
                        |> Maybe.map (prettyList "Tx plutus V1 scripts:" prettyBytes)
                    , tx.witnessSet.plutusV2Script
                        |> Maybe.map (prettyList "Tx plutus V2 scripts:" prettyBytes)
                    , tx.witnessSet.redeemer
                        |> Maybe.map (prettyList "Tx redeemers:" prettyRedeemer)
                    , Nothing -- TODO: plutusData
                    ]

        -- Pretty print auxiliary data
        auxiliaryData =
            case tx.auxiliaryData of
                Nothing ->
                    []

                Just auxData ->
                    List.concat <|
                        [ prettyList "Tx metadata:" prettyMetadata auxData.labels
                        , prettyList "Tx native scripts in auxiliary data:" (prettyScript << Script.Native) auxData.nativeScripts
                        , prettyList "Tx plutus V1 scripts in auxiliary data:" prettyBytes auxData.plutusV1Scripts
                        , prettyList "Tx plutus V2 scripts in auxiliary data:" prettyBytes auxData.plutusV2Scripts
                        ]
    in
    List.concat [ body, witnessSet, auxiliaryData ]
        |> String.join "\n"
