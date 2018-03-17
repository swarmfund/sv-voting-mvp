module SecureVote.SPAs.DelegationUI.Update exposing (..)

-- import SecureVote.Eth.Tasks exposing (getVotersForDlgtTask)

import Dict
import Element.Input exposing (dropMenu, menu, select, selected, updateSelection)
import Json.Encode as E
import Maybe.Extra exposing ((?))
import SecureVote.Eth.Update as EthUpdate exposing (ethUpdate)
import SecureVote.Eth.Web3 exposing (performContractRead, setGlobalDelegationImpl, setTokenDelegationImpl)
import SecureVote.SPAs.DelegationUI.Helpers exposing (..)
import SecureVote.SPAs.DelegationUI.Model exposing (Model, Web3Model, initWeb3Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (..)
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationType(..))
import SecureVote.SmartContracts.Delegation exposing (getVotersForDlgtRecursive, getVotersForDlgtTask, viewDelegationArgs)
import SecureVote.Tokens.Types exposing (tcChoiceToAddr, tcChoiceToAddrM)
import SecureVote.Utils.Msgs exposing (msgOrErr)
import SecureVote.Utils.Ports exposing (mkCarry)
import Task


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            model ! []

        SetStrField k v ->
            let
                sf =
                    if v == "" then
                        Dict.remove k model.strFields
                    else
                        Dict.insert k v model.strFields
            in
            update OnFieldUpdate { model | strFields = sf }

        SetBoolField k v ->
            update OnFieldUpdate { model | boolFields = Dict.insert k v model.boolFields }

        ToggleBoolField k ->
            update OnFieldUpdate { model | boolFields = Dict.update k (Maybe.withDefault False >> not >> Just) model.boolFields }

        SelectTokenContract selectMsg ->
            update OnFieldUpdate { model | tokenConAddr = updateSelection selectMsg model.tokenConAddr }

        SetDelegationType delType ->
            { model | delType = Just delType } ! []

        OnFieldUpdate ->
            let
                modelDelTx =
                    model.delTx

                newDelegationTx =
                    { modelDelTx | to = model.delegationAddr }

                delegateAddr =
                    getStrField model setDelegateAddrId ? ""

                tokenAddr =
                    tcChoiceToAddr <| selected model.tokenConAddr

                cmd =
                    case model.delType of
                        Just Global ->
                            setGlobalDelegationImpl
                                { delegationABI = model.delegationABI
                                , contractAddr = model.delegationAddr
                                , delegateAddr = delegateAddr
                                }

                        Just Token ->
                            setTokenDelegationImpl
                                { delegationABI = model.delegationABI
                                , contractAddr = model.delegationAddr
                                , delegateAddr = delegateAddr
                                , tokenContract = tokenAddr
                                }

                        _ ->
                            Cmd.none
            in
            { model | delTx = newDelegationTx } ! [ cmd ]

        ReceivedPayload payload ->
            let
                modelDelTx =
                    model.delTx

                newDelegationTx =
                    { modelDelTx | data = payload }
            in
            { model | delTx = newDelegationTx } ! []

        ViewDlgtResp r ->
            { model | viewDlgtResp = r } ! []

        GetVotersForDlgt delegatee ->
            let
                cmdExtra =
                    case tcChoiceToAddrM <| selected model.tokenConAddr of
                        Just addr ->
                            [ getVotersForDlgtRecursive
                                { scAddr = model.delegationAddr
                                , abi = model.delegationABI
                                , dlgtAddr = delegatee
                                , tknAddr = addr
                                , carryVtrs = []
                                }
                                |> Task.attempt (msgOrErr (GotDlgtAllVotersForContract addr) LogErr)
                            ]

                        _ ->
                            []
            in
            { model | votersForDlgtByToken = Dict.singleton "Loading..." [], votersForDlgtRecursive = Nothing }
                ! ([ Task.attempt (msgOrErr GotVotersForDlgt LogErr) <|
                        getVotersForDlgtTask { scAddr = model.delegationAddr, abi = model.delegationABI, dlgtAddr = delegatee }
                   ]
                    ++ cmdExtra
                  )

        GotVotersForDlgt d ->
            { model | votersForDlgtByToken = d } ! []

        GotDlgtAllVotersForContract addr vs ->
            { model | votersForDlgtRecursive = Just ( addr, vs ) } ! []

        MMsg msgs ->
            case msgs of
                [] ->
                    model ! []

                msg :: msgs_ ->
                    let
                        ( m_, cmds_ ) =
                            update (MMsg msgs_) model

                        ( m__, cmds__ ) =
                            update msg m_
                    in
                    m__ ! [ cmds_, cmds__ ]

        LogErr err ->
            { model | errors = Debug.log "Update: got err => " err :: model.errors } ! []

        Web3 ethMsg ->
            ethUpdate Web3 ethMsg model


checkVoterDlgtPairs : Model -> List ( String, String ) -> Cmd Msg
checkVoterDlgtPairs model ps =
    Cmd.batch <|
        List.map
            (\( v, t ) ->
                performContractRead
                    { carry = mkCarry (E.string ethCheckDelegationId)
                    , addr = model.delegationAddr
                    , abi = model.delegationABI
                    , args = viewDelegationArgs ( v, t )
                    , method = "resolveDelegation"
                    }
            )
            ps
