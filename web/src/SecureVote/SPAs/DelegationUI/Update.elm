module SecureVote.SPAs.DelegationUI.Update exposing (..)

import Dict
import Element.Input exposing (dropMenu, menu, select, selected, updateSelection)
import Maybe.Extra exposing ((?))
import SecureVote.Ballots.Types exposing (emptyBSpec01)
import SecureVote.Eth.Web3 exposing (setDelegateData)
import SecureVote.SPAs.DelegationUI.Components.Input exposing (genDropSelect)
import SecureVote.SPAs.DelegationUI.Model exposing (Model, Web3Model, initWeb3Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (..)
import Task
import Web3.Types exposing (Error(Error), Sha3(Sha3))
import Web3.Utils exposing (sha3)


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
            { model | strFields = sf } ! []

        SetBoolField k v ->
            { model | boolFields = Dict.insert k v model.boolFields } ! []

        SelectTokenContract selectMsg ->
            { model | tokenConAddr = updateSelection selectMsg model.tokenConAddr } ! []

        SetDelegationType delType ->
            { model | delType = Just delType } ! []

        GetDelegationPayload { delegateAddr, tokenAddr } ->
            let
                modelDelTx =
                    model.delTx

                newDelegationTx =
                    { modelDelTx | to = delegateAddr }
            in
            { model | delTx = newDelegationTx }
                ! [ setDelegateData
                        { delegationABI = model.delABI
                        , contractAddr = model.delConAddr
                        , delegateAddr = delegateAddr
                        , tokenContract = tokenAddr
                        }
                  ]

        GotDelegationPayload payload ->
            let
                modelDelTx =
                    model.delTx

                newDelegationTx =
                    { modelDelTx | data = payload }
            in
            { model | delTx = newDelegationTx } ! []

        LogErr err ->
            { model | errors = err :: model.errors } ! []
