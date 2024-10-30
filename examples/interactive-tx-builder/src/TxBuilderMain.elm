port module TxBuilderMain exposing (main)

import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Bytes.Map as Map exposing (BytesMap)
import Cardano exposing (SpendSource(..), TxIntent(..), WitnessSource(..))
import Cardano.Address as Address exposing (Address(..), StakeAddress, StakeCredential)
import Cardano.Cip30 as Cip30
import Cardano.Data as Data exposing (Data)
import Cardano.Gov as Gov exposing (ActionId, Drep, Vote(..), Voter)
import Cardano.MultiAsset as MultiAsset exposing (AssetName, MultiAsset, PolicyId)
import Cardano.Transaction as Transaction exposing (FeeParameters, PoolId, Transaction)
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference, TransactionId)
import Cardano.Value as Value exposing (Value)
import Dict exposing (Dict)
import Dict.Any
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Integer exposing (Integer)
import Json.Decode as JD
import Natural exposing (Natural)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- PORTS


port toWallet : JD.Value -> Cmd msg


port fromWallet : (JD.Value -> msg) -> Sub msg



-- MODEL


type alias Model =
    { appState : AppState
    }


type alias AppState =
    { cart : TxCart
    , currentCost : TxCost
    , wallets : Dict String WalletStatus
    , tempTxElement : Maybe TxElementInProgress
    , errors : List String
    , localStateUtxos : Utxo.RefDict Output
    , txSubmissionState : TxSubmissionState

    -- Give short names to known things for pretty printing
    , prettyConfig : PrettyConfig

    -- Keeping a Tx history to be applied to wallet utxos
    -- to be able to keep a consistent localStateUtxos.
    , txHistory : List ( Bytes TransactionId, Transaction )
    }


type alias TxCart =
    List TxElement


type TxElement
    = Transfer { from : Address, to : Address, value : Value }
    | WithdrawRewards { stakeAddress : StakeAddress, amount : Natural }
    | SpendFromContract { contractAddress : Address, utxo : OutputReference, redeemer : Data }
    | SendToContract { contractAddress : Address, value : Value, datum : Data }
    | MintBurn { policyId : Bytes PolicyId, assets : BytesMap AssetName Integer }
    | DelegateStakeToPool { delegator : StakeAddress, poolId : Bytes PoolId }
    | DelegateStakeToDRep { delegator : StakeAddress, dRep : Drep }
    | Vote { voter : Voter, proposalId : ActionId, vote : Vote }


type alias TxCost =
    { ada : Natural
    , feeBreakdown :
        { txSizeFee : Natural
        , scriptExecFee : Natural
        , refScriptSizeFee : Natural
        }
    , breakdown :
        { txSize : Int
        , refContractSize : Int
        , mem : Natural
        , cpu : Natural
        }
    }


txCostInit : TxCost
txCostInit =
    { ada = Natural.zero
    , feeBreakdown =
        { txSizeFee = Natural.zero
        , scriptExecFee = Natural.zero
        , refScriptSizeFee = Natural.zero
        }
    , breakdown =
        { txSize = 0
        , refContractSize = 0
        , mem = Natural.zero
        , cpu = Natural.zero
        }
    }


type TxSubmissionState
    = NoTx
    | TxSigning Cip30.Wallet Transaction
    | TxSubmitting Cip30.Wallet Transaction


type WalletStatus
    = WalletJustDiscovered Cip30.WalletDescriptor
    | WalletLoading { wallet : Cip30.Wallet, utxos : Utxo.RefDict Output }
    | WalletConnected { wallet : Cip30.Wallet, utxos : Utxo.RefDict Output, changeAddress : Address }


walletUtxos : WalletStatus -> Utxo.RefDict Output
walletUtxos walletStatus =
    case walletStatus of
        WalletConnected { utxos } ->
            utxos

        _ ->
            Utxo.emptyRefDict


type TxElementInProgress
    = TransferInProgress TransferWizardState
    | WithdrawRewardsInProgress WithdrawRewardsWizardState
    | SpendFromContractInProgress SpendFromContractWizardState
    | SendToContractInProgress SendToContractWizardState
    | MintBurnInProgress MintBurnWizardState
    | DelegateStakeToPoolInProgress DelegateStakeToPoolWizardState
    | DelegateStakeToDRepInProgress DelegateStakeToDRepWizardState
    | VoteInProgress VoteWizardState


type alias TransferWizardState =
    { from : Maybe Address
    , to : Maybe Address
    , adaAmount : Natural
    , selectedTokens : List SelectedToken
    }


type alias SelectedToken =
    { policyId : Bytes PolicyId
    , assetName : Bytes AssetName
    , amount : Natural
    }



-- TODO: Define other wizard states


type alias WithdrawRewardsWizardState =
    {}


type alias SpendFromContractWizardState =
    {}


type alias SendToContractWizardState =
    {}


type alias MintBurnWizardState =
    {}


type alias DelegateStakeToPoolWizardState =
    {}


type alias DelegateStakeToDRepWizardState =
    {}


type alias VoteWizardState =
    {}


type Page
    = WalletSelectionPage
    | TxBuilderPage



-- TODO: Define DRep, Voter, ProposalId, and VoteOption types


init : () -> ( Model, Cmd Msg )
init _ =
    ( { appState =
            { cart = []
            , currentCost = txCostInit
            , wallets = Dict.empty
            , tempTxElement = Nothing
            , errors = []
            , localStateUtxos = Utxo.emptyRefDict
            , txSubmissionState = NoTx
            , prettyConfig = emptyPrettyConfig
            , txHistory = []
            }
      }
    , toWallet (Cip30.encodeRequest Cip30.discoverWallets)
    )



-- UPDATE


type Msg
    = WalletMsg JD.Value
    | StartNewElement TxElementType
    | UpdateElementWizard TxElementWizardMsg
    | AddElementToCart
    | CancelElementWizard
    | RemoveElementFromCart Int
    | ClearCart
    | ConnectWallet Cip30.WalletDescriptor
    | SubmitTxClicked
    | TxFinalizationResult (Result Cardano.TxFinalizationError Transaction)


type TxElementType
    = TransferType
    | WithdrawRewardsType
    | SpendFromContractType
    | SendToContractType
    | MintBurnType
    | DelegateStakeToPoolType
    | DelegateStakeToDRepType
    | VoteType


type TxElementWizardMsg
    = UpdateTransferWizard TransferWizardMsg



-- TODO: Add other wizard message types


type TransferWizardMsg
    = SetTransferFrom String
    | SetTransferTo String
    | SetAdaAmount String
    | SelectToken (Bytes PolicyId) (Bytes AssetName)
    | SetTokenAmount (Bytes PolicyId) (Bytes AssetName) String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ appState } as model) =
    case msg of
        WalletMsg value ->
            handleWalletMsg value model

        StartNewElement elementType ->
            ( { model | appState = startNewElement elementType model.appState }, Cmd.none )

        UpdateElementWizard wizardMsg ->
            case ( model.appState.tempTxElement, wizardMsg ) of
                ( Just (TransferInProgress state), UpdateTransferWizard transferWizardMsg ) ->
                    let
                        newState =
                            updateTransferWizard transferWizardMsg state
                    in
                    ( { model | appState = { appState | tempTxElement = Just (TransferInProgress newState) } }, Cmd.none )

                -- ... (handle other wizard types)
                ( Just _, _ ) ->
                    Debug.todo "handle other wizard types"

                ( Nothing, _ ) ->
                    ( model, Cmd.none )

        AddElementToCart ->
            let
                ( newAppState, cmd ) =
                    addElementToCart model.appState
            in
            ( { model | appState = newAppState }, cmd )

        CancelElementWizard ->
            ( { model | appState = cancelElementWizard model.appState }, Cmd.none )

        RemoveElementFromCart index ->
            ( { model | appState = removeElementFromCart index model.appState }, Cmd.none )

        ClearCart ->
            ( { model | appState = clearCart model.appState }, Cmd.none )

        ConnectWallet descriptor ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = descriptor.id, extensions = [] })) )

        SubmitTxClicked ->
            let
                ( newAppState, cmd ) =
                    signAndSubmitTxInCart model.appState
            in
            ( { model | appState = newAppState }, cmd )

        TxFinalizationResult result ->
            handleTxFinalizationResult result model



--


handleWalletMsg : JD.Value -> Model -> ( Model, Cmd Msg )
handleWalletMsg value ({ appState } as model) =
    case JD.decodeValue Cip30.responseDecoder value of
        Ok (Cip30.AvailableWallets wallets) ->
            ( { model | appState = updateWallets wallets model.appState }, Cmd.none )

        Ok (Cip30.EnabledWallet wallet) ->
            let
                updatedWallets =
                    Dict.update (Cip30.walletDescriptor wallet).id
                        (Maybe.map (\_ -> WalletLoading { wallet = wallet, utxos = Utxo.emptyRefDict }))
                        model.appState.wallets

                updatedAppState =
                    { appState | wallets = updatedWallets }
            in
            ( { model | appState = updatedAppState }
            , toWallet (Cip30.encodeRequest (Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing }))
            )

        Ok (Cip30.ApiResponse { walletId } (Cip30.WalletUtxos utxos)) ->
            let
                updatedWallets =
                    Dict.update walletId
                        (Maybe.map
                            (\status ->
                                case status of
                                    WalletLoading data ->
                                        WalletLoading { data | utxos = Utxo.refDictFromList utxos }

                                    _ ->
                                        status
                            )
                        )
                        model.appState.wallets

                updatedAppState =
                    { appState | wallets = updatedWallets }
            in
            ( { model | appState = updatedAppState }
            , case Dict.get walletId updatedAppState.wallets of
                Just (WalletLoading { wallet }) ->
                    toWallet (Cip30.encodeRequest (Cip30.getChangeAddress wallet))

                _ ->
                    Cmd.none
            )

        Ok (Cip30.ApiResponse { walletId } (Cip30.ChangeAddress address)) ->
            let
                updatedWallets =
                    Dict.update walletId
                        (Maybe.map
                            (\status ->
                                case status of
                                    WalletLoading { wallet, utxos } ->
                                        WalletConnected { wallet = wallet, utxos = utxos, changeAddress = address }

                                    _ ->
                                        status
                            )
                        )
                        model.appState.wallets

                updatedAppState =
                    { appState
                        | wallets = updatedWallets
                        , localStateUtxos = recomputeLocalStateUtxos updatedWallets appState.txHistory
                        , prettyConfig = addPrettyAddress address "Me" appState.prettyConfig
                    }
            in
            ( { model | appState = updatedAppState }, Cmd.none )

        Err error ->
            ( { model | appState = { appState | errors = Debug.toString error :: model.appState.errors } }, Cmd.none )

        _ ->
            ( model, Cmd.none )


recomputeLocalStateUtxos : Dict String WalletStatus -> List ( Bytes TransactionId, Transaction ) -> Utxo.RefDict Output
recomputeLocalStateUtxos wallets txHistory =
    let
        allWalletsUtxos =
            Dict.foldl
                (\_ w state -> Cardano.addUtxosToLocalState (Dict.Any.toList <| walletUtxos w) state)
                Utxo.emptyRefDict
                wallets

        updateStateWithTx ( txId, tx ) state =
            Cardano.updateLocalState txId tx state
                |> (\{ updatedState } -> updatedState)
    in
    -- Walk through txHistory starting with oldest transactions (end of list)
    List.foldr updateStateWithTx allWalletsUtxos txHistory


startNewElement : TxElementType -> AppState -> AppState
startNewElement elementType appState =
    { appState | tempTxElement = Just (initializeWizard elementType) }


initializeWizard : TxElementType -> TxElementInProgress
initializeWizard elementType =
    case elementType of
        TransferType ->
            TransferInProgress { from = Nothing, to = Nothing, adaAmount = Natural.zero, selectedTokens = [] }

        -- TODO: Initialize other wizard types
        _ ->
            TransferInProgress { from = Nothing, to = Nothing, adaAmount = Natural.zero, selectedTokens = [] }


updateElementWizard : TxElementWizardMsg -> AppState -> AppState
updateElementWizard wizardMsg appState =
    case appState.tempTxElement of
        Just element ->
            { appState | tempTxElement = Just (updateWizard wizardMsg element) }

        Nothing ->
            appState


updateWizard : TxElementWizardMsg -> TxElementInProgress -> TxElementInProgress
updateWizard wizardMsg element =
    case ( wizardMsg, element ) of
        ( UpdateTransferWizard transferMsg, TransferInProgress state ) ->
            TransferInProgress (updateTransferWizard transferMsg state)

        -- TODO: Handle other wizard types
        _ ->
            element


updateTransferWizard : TransferWizardMsg -> TransferWizardState -> TransferWizardState
updateTransferWizard msg state =
    case msg of
        SetTransferFrom addressStr ->
            { state | from = Address.fromBytes <| Bytes.fromHexUnchecked addressStr }

        SetTransferTo addressStr ->
            { state | to = Address.fromBytes <| Bytes.fromHexUnchecked addressStr }

        SetAdaAmount adaStr ->
            { state | adaAmount = Natural.fromString adaStr |> Maybe.withDefault Natural.zero }

        SelectToken policyId assetName ->
            let
                newToken =
                    SelectedToken policyId assetName Natural.zero

                newSelectedTokens =
                    if List.any (\t -> t.policyId == policyId && t.assetName == assetName) state.selectedTokens then
                        List.filter (\t -> not (t.policyId == policyId && t.assetName == assetName)) state.selectedTokens

                    else
                        newToken :: state.selectedTokens
            in
            { state | selectedTokens = newSelectedTokens }

        SetTokenAmount policyId assetName amountStr ->
            let
                updateAmount token =
                    if token.policyId == policyId && token.assetName == assetName then
                        { token | amount = Maybe.withDefault Natural.zero (Natural.fromString amountStr) }

                    else
                        token

                newSelectedTokens =
                    List.map updateAmount state.selectedTokens
            in
            { state | selectedTokens = newSelectedTokens }


{-| Add to cart the current element being edited.
-}
addElementToCart : AppState -> ( AppState, Cmd Msg )
addElementToCart appState =
    case appState.tempTxElement of
        Just element ->
            case wizardToTxElement element of
                -- If the wizard is fully completed, it should yield a Tx element
                Just txElement ->
                    -- Try to validate the Tx element and to add it to the cart if valid
                    case validateAndAddTxElement txElement appState of
                        Ok newAppState ->
                            ( { newAppState | tempTxElement = Nothing, errors = [] }, Cmd.none )

                        Err error ->
                            ( { appState | errors = [ error ] }, Cmd.none )

                -- If the wizard forms are incomplete, let the user know
                Nothing ->
                    ( { appState | errors = [ "Incomplete element" ] }, Cmd.none )

        -- Do nothing if we aren’t editing any new element currently
        Nothing ->
            ( appState, Cmd.none )


wizardToTxElement : TxElementInProgress -> Maybe TxElement
wizardToTxElement element =
    case element of
        TransferInProgress { from, to, adaAmount, selectedTokens } ->
            case ( from, to ) of
                ( Just fromAddr, Just toAddr ) ->
                    Just <|
                        Transfer
                            { from = fromAddr
                            , to = toAddr
                            , value =
                                Value.add
                                    (Value.onlyLovelace adaAmount)
                                    (Value.fromTokenList selectedTokens)
                            }

                _ ->
                    Nothing

        -- TODO: Handle other wizard types
        _ ->
            Nothing


validateAndAddTxElement : TxElement -> AppState -> Result String AppState
validateAndAddTxElement element appState =
    let
        newCart =
            element :: appState.cart

        generatedIntents =
            cartToTxIntents newCart
                |> Debug.log "generatedIntents"
    in
    case Cardano.finalize appState.localStateUtxos [] generatedIntents of
        Ok tx ->
            Ok
                { appState
                    | cart = newCart
                    , currentCost = computeCost Transaction.defaultTxFeeParams tx
                }

        Err error ->
            Err (Debug.toString error)


cartToTxIntents : TxCart -> List TxIntent
cartToTxIntents cart =
    List.concatMap elementToTxIntents cart


elementToTxIntents : TxElement -> List TxIntent
elementToTxIntents element =
    case element of
        Transfer { from, to, value } ->
            [ Spend (FromWallet from value)
            , SendTo to value
            ]

        WithdrawRewards { stakeAddress, amount } ->
            [ Cardano.WithdrawRewards
                { stakeCredential = stakeAddress
                , amount = amount
                , scriptWitness = Nothing
                }
            ]

        SpendFromContract { contractAddress, utxo, redeemer } ->
            [ Spend
                (FromPlutusScript
                    { spentInput = utxo
                    , datumWitness = Nothing -- Assuming datum is inline, adjust if needed
                    , plutusScriptWitness =
                        { script = Debug.todo "Need to provide script version and source"
                        , redeemerData = \_ -> redeemer
                        , requiredSigners = [] -- Add required signers if needed
                        }
                    }
                )
            ]

        SendToContract { contractAddress, value, datum } ->
            [ SendToOutput
                { address = contractAddress
                , amount = value
                , datumOption = Just (DatumValue datum)
                , referenceScript = Nothing
                }
            ]

        MintBurn { policyId, assets } ->
            [ Cardano.MintBurn
                { policyId = policyId
                , assets = assets
                , scriptWitness = Debug.todo "Need to provide script witness"
                }
            ]

        DelegateStakeToPool { delegator, poolId } ->
            [ IssueCertificate
                (Cardano.DelegateStake
                    { delegator = Cardano.WithKey (Address.extractCredentialHash delegator.stakeCredential)
                    , poolId = poolId
                    }
                )
            ]

        DelegateStakeToDRep { delegator, dRep } ->
            [ IssueCertificate
                (Cardano.DelegateVotes
                    { delegator = Cardano.WithKey (Address.extractCredentialHash delegator.stakeCredential)
                    , drep = dRepToCredential dRep
                    }
                )
            ]

        Vote { voter, proposalId, vote } ->
            [ Cardano.Vote (voterToVoterWitness voter) [ { actionId = proposalId, vote = vote } ] ]


dRepToCredential : Drep -> Address.Credential
dRepToCredential dRep =
    case dRep of
        Gov.DrepCredential cred ->
            cred

        Gov.AlwaysAbstain ->
            Debug.todo "Handle AlwaysAbstain"

        Gov.AlwaysNoConfidence ->
            Debug.todo "Handle AlwaysNoConfidence"


voterToVoterWitness : Voter -> Cardano.VoterWitness
voterToVoterWitness _ =
    Debug.todo "voterToVoterWitness"


cancelElementWizard : AppState -> AppState
cancelElementWizard appState =
    { appState | tempTxElement = Nothing }


removeElementFromCart : Int -> AppState -> AppState
removeElementFromCart index appState =
    let
        newCart =
            List.take index appState.cart ++ List.drop (index + 1) appState.cart

        generatedIntents =
            cartToTxIntents newCart
    in
    case Cardano.finalize appState.localStateUtxos [] generatedIntents of
        Ok tx ->
            { appState
                | cart = newCart
                , currentCost = computeCost Transaction.defaultTxFeeParams tx
            }

        Err err ->
            { appState | errors = [ Debug.toString err ] }


computeCost : FeeParameters -> Transaction -> TxCost
computeCost feeParams tx =
    let
        refScriptBytes =
            -- TODO: have actual value here
            0

        ({ txSizeFee, scriptExecFee, refScriptSizeFee } as feeBreakdown) =
            Transaction.computeFees feeParams { refScriptBytes = refScriptBytes } tx

        { totalSteps, totalMem } =
            Transaction.extractTotalExecCost tx
    in
    { ada = Natural.add txSizeFee <| Natural.add scriptExecFee refScriptSizeFee
    , feeBreakdown = feeBreakdown
    , breakdown =
        { txSize = Bytes.width (Transaction.serialize tx)
        , refContractSize = refScriptBytes
        , mem = totalMem
        , cpu = totalSteps
        }
    }


clearCart : AppState -> AppState
clearCart appState =
    { appState | cart = [], currentCost = txCostInit }


updateWallets : List Cip30.WalletDescriptor -> AppState -> AppState
updateWallets wallets appState =
    { appState | wallets = Dict.fromList (List.map (\w -> ( w.id, WalletJustDiscovered w )) wallets) }


signAndSubmitTxInCart : AppState -> ( AppState, Cmd Msg )
signAndSubmitTxInCart appState =
    case Cardano.finalize appState.localStateUtxos [] (cartToTxIntents appState.cart) of
        Ok tx ->
            let
                -- TODO: figure out which wallet to sign this with
                -- Remove all fake signatures before sending to wallet
                cleanTx =
                    Transaction.updateSignatures (\_ -> Nothing) tx
            in
            -- Sign and submit transaction
            Debug.todo "sign"

        Err error ->
            -- TODO: Handle error
            Debug.todo "sign"


handleTxFinalizationResult : Result Cardano.TxFinalizationError Transaction -> Model -> ( Model, Cmd Msg )
handleTxFinalizationResult result model =
    -- TODO: Handle transaction finalization result
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    fromWallet WalletMsg



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewWalletSection model.appState
        , viewTxBuilder model.appState
        ]


viewWalletSection : AppState -> Html Msg
viewWalletSection appState =
    div []
        [ text "Available Wallets:"
        , div [] (List.map viewWalletOption (Dict.values appState.wallets))
        ]


viewWalletOption : WalletStatus -> Html Msg
viewWalletOption status =
    case status of
        WalletJustDiscovered descriptor ->
            button [ onClick (ConnectWallet descriptor) ] [ text descriptor.name ]

        WalletLoading _ ->
            text "Loading..."

        WalletConnected _ ->
            text "Connected"


viewTxBuilder : AppState -> Html Msg
viewTxBuilder appState =
    div []
        [ viewCart appState
        , viewTxCost appState.currentCost
        , viewErrors appState.errors
        , viewElementWizard appState appState.tempTxElement
        ]


viewCart : AppState -> Html Msg
viewCart appState =
    if List.isEmpty appState.cart then
        text ""

    else
        div []
            [ text "Transaction Cart:"
            , div [] (List.indexedMap (viewCartElement appState.prettyConfig) appState.cart)
            , button [ onClick ClearCart ] [ text "Clear Cart" ]
            , button [ onClick SubmitTxClicked ] [ text "Submit Tx" ]
            ]


viewCartElement : PrettyConfig -> Int -> TxElement -> Html Msg
viewCartElement prettyConfig index element =
    div []
        [ viewPrettyTxElement prettyConfig element
        , button [ onClick (RemoveElementFromCart index) ] [ text "Remove" ]
        ]


viewPrettyTxElement : PrettyConfig -> TxElement -> Html msg
viewPrettyTxElement config element =
    case element of
        Transfer { from, to, value } ->
            let
                prettyFromTo =
                    prettyAddrWithConfig config from
                        ++ " -> "
                        ++ prettyAddrWithConfig config to
                        ++ " : "
            in
            case prettyValue value of
                [] ->
                    Html.pre [] [ text <| prettyFromTo ++ "nothing" ]

                [ singleLine ] ->
                    Html.pre [] [ text <| prettyFromTo ++ singleLine ]

                firstLine :: otherLines ->
                    (prettyFromTo ++ firstLine)
                        :: otherLines
                        |> String.join "\n"
                        |> (\s -> Html.pre [] [ text s ])

        _ ->
            Debug.todo "viewPrettyTxElement"


prettyAddrWithConfig : PrettyConfig -> Address -> String
prettyAddrWithConfig config addr =
    case Dict.Any.get addr config.knownAddresses of
        Just shortname ->
            shortname

        Nothing ->
            Debug.todo "prettyAddrWithConfig"


viewTxCost : TxCost -> Html Msg
viewTxCost cost =
    let
        adaStr amount =
            "₳" ++ Natural.toString amount
    in
    div []
        [ text
            ("Total Cost: "
                ++ adaStr cost.ada
                ++ " (Tx size: "
                ++ adaStr cost.feeBreakdown.txSizeFee
                ++ ", script size: "
                ++ adaStr cost.feeBreakdown.refScriptSizeFee
                ++ ", script exec: "
                ++ adaStr cost.feeBreakdown.scriptExecFee
                ++ ")"
            )
        , div [] [ text ("Tx Size: " ++ String.fromInt cost.breakdown.txSize ++ " B / (max) 16.384 kB") ]
        , div [] [ text ("Ref Contract Size: " ++ String.fromInt cost.breakdown.refContractSize ++ " B") ]
        , div [] [ text ("Memory: " ++ Natural.toString cost.breakdown.mem ++ " units / (max) 14000 k units") ]
        , div [] [ text ("CPU: " ++ Natural.toString cost.breakdown.cpu ++ " steps / (max) 10000 M steps") ]
        ]


viewErrors : List String -> Html Msg
viewErrors errors =
    div [] (List.map (\error -> div [] [ text error ]) errors)


viewElementWizard : AppState -> Maybe TxElementInProgress -> Html Msg
viewElementWizard appState maybeElement =
    case maybeElement of
        Just element ->
            div []
                [ viewWizard appState element
                , button [ onClick AddElementToCart ] [ text "Add to cart" ]
                , button [ onClick CancelElementWizard ] [ text "Cancel" ]
                ]

        Nothing ->
            div []
                [ button [ onClick (StartNewElement TransferType) ] [ text "New Transfer" ]

                -- TODO: Add buttons for other element types
                ]


viewWizard : AppState -> TxElementInProgress -> Html Msg
viewWizard appState element =
    case element of
        TransferInProgress state ->
            viewTransferWizard state appState

        -- TODO: Add views for other wizard types
        _ ->
            text "Unsupported element type"


viewTransferWizard : TransferWizardState -> AppState -> Html Msg
viewTransferWizard state appState =
    div []
        [ viewTransferFrom state
        , viewTransferTo state
        , viewTokenSelection state appState
        ]


viewTransferFrom : TransferWizardState -> Html Msg
viewTransferFrom state =
    div []
        [ text "From Address: "
        , Html.input
            [ Html.Events.onInput (\s -> UpdateElementWizard (UpdateTransferWizard (SetTransferFrom s)))
            , Html.Attributes.value (Maybe.withDefault "" <| Maybe.map prettyAddr state.from)
            ]
            []
        ]


viewTransferTo : TransferWizardState -> Html Msg
viewTransferTo state =
    div []
        [ text "To Address: "
        , Html.input
            [ Html.Events.onInput (\s -> UpdateElementWizard (UpdateTransferWizard (SetTransferTo s)))
            , Html.Attributes.value (Maybe.withDefault "" <| Maybe.map prettyAddr state.to)
            ]
            []
        ]


viewTokenSelection : TransferWizardState -> AppState -> Html Msg
viewTokenSelection state appState =
    case state.from of
        Nothing ->
            text "Please enter a valid 'From' address to select tokens."

        Just fromAddress ->
            let
                utxosAtAddress =
                    Dict.Any.filter (\_ output -> output.address == fromAddress) appState.localStateUtxos

                totalValue =
                    List.foldl (\output -> Value.add output.amount) Value.zero (Dict.Any.values utxosAtAddress)

                availableTokens =
                    Map.toList totalValue.assets
                        |> List.concatMap (\( p, assets ) -> List.map (Tuple.pair p) (Map.keys assets))
            in
            if Dict.Any.isEmpty utxosAtAddress then
                div [] [ text "There is nothing at this address, maybe you forgot to connect your wallet?" ]

            else
                div []
                    [ text "Ada Lovelace: "
                    , Html.input
                        [ Html.Events.onInput (\s -> UpdateElementWizard (UpdateTransferWizard (SetAdaAmount s)))
                        , Html.Attributes.value (Natural.toString state.adaAmount)
                        ]
                        []
                    , div [] [ text "Select Tokens:" ]
                    , div [] (List.map (viewTokenOption state) availableTokens)
                    ]


viewTokenOption : TransferWizardState -> ( Bytes PolicyId, Bytes AssetName ) -> Html Msg
viewTokenOption state ( policyId, assetName ) =
    let
        isSelected =
            List.any (\t -> t.policyId == policyId && t.assetName == assetName) state.selectedTokens

        selectedToken =
            List.filter (\t -> t.policyId == policyId && t.assetName == assetName) state.selectedTokens |> List.head
    in
    div []
        [ checkbox (UpdateElementWizard (UpdateTransferWizard (SelectToken policyId assetName))) isSelected
        , text (Bytes.toHex policyId ++ "  " ++ prettyBytes assetName)
        , Html.input
            [ Html.Events.onInput (\s -> UpdateElementWizard (UpdateTransferWizard (SetTokenAmount policyId assetName s)))
            , Html.Attributes.value (Maybe.map (.amount >> Natural.toString) selectedToken |> Maybe.withDefault "")
            , disabled (not isSelected)
            ]
            []
        ]


checkbox : msg -> Bool -> Html msg
checkbox msg isChecked =
    Html.input
        [ Html.Attributes.type_ "checkbox"
        , onClick msg
        , Html.Attributes.checked isChecked
        ]
        []



-- Helper functions


type alias PrettyConfig =
    { knownAddresses : Address.Dict String
    }


emptyPrettyConfig : PrettyConfig
emptyPrettyConfig =
    { knownAddresses = Address.emptyDict
    }


addPrettyAddress : Address -> String -> PrettyConfig -> PrettyConfig
addPrettyAddress address shortname config =
    { config | knownAddresses = Dict.Any.insert address shortname config.knownAddresses }


prettyAddr : Address -> String
prettyAddr address =
    case address of
        Byron b ->
            prettyBytes b

        Shelley { paymentCredential, stakeCredential } ->
            [ Just "Addr:", Just (prettyCred paymentCredential), Maybe.map prettyStakeCred stakeCredential ]
                |> List.filterMap identity
                |> String.join " "

        Reward stakeAddr ->
            "StakeAddr:" ++ prettyCred stakeAddr.stakeCredential


prettyStakeCred : StakeCredential -> String
prettyStakeCred stakeCred =
    case stakeCred of
        Address.InlineCredential cred ->
            "stake:" ++ prettyCred cred

        Address.PointerCredential _ ->
            "stake:PointerAddr"


prettyCred cred =
    case cred of
        Address.VKeyHash b ->
            "key:" ++ prettyBytes b

        Address.ScriptHash b ->
            "script:" ++ prettyBytes b


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
                                [ prettyBytes name
                                , "(" ++ prettyShortBytes 8 policyId ++ ")"
                                , toStr amount
                                ]
                        )
            )


prettyShortBytes length b =
    case Bytes.toText b of
        Nothing ->
            Bytes.toHex b
                |> shortenHex length

        Just text ->
            let
                isLikelyAscii char =
                    Char.toCode char < 128
            in
            if String.all isLikelyAscii text then
                text

            else
                Bytes.toHex b
                    |> shortenHex length


shortenHex length hex =
    if String.length hex <= length then
        hex

    else
        let
            halfLength =
                length // 2
        in
        String.slice 0 halfLength hex
            ++ ".."
            ++ String.slice -halfLength (String.length hex) hex


prettyBytes b =
    case Bytes.toText b of
        Nothing ->
            Bytes.toHex b

        Just text ->
            let
                isLikelyAscii char =
                    Char.toCode char < 128
            in
            if String.all isLikelyAscii text then
                text

            else
                Bytes.toHex b


indent spaces str =
    String.repeat spaces " " ++ str
