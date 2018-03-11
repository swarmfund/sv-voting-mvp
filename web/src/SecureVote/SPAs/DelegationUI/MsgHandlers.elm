module SecureVote.SPAs.DelegationUI.MsgHandlers exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as E
import List.Extra
import RemoteData exposing (RemoteData(..))
import Result.Extra
import SecureVote.Eth.Types exposing (ReadResponse, ReadResponseWCarry)
import SecureVote.SPAs.DelegationUI.Helpers exposing (ethCheckDelegationId)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (..)
import SecureVote.SPAs.DelegationUI.Types exposing (..)
import SecureVote.SmartContracts.Delegation exposing (decDelegationResp, decDelegatorsOfResp)
import SecureVote.Utils.DecodeP exposing (strInt)
import SecureVote.Utils.Msgs exposing (msgOrErr)


errHelper : String -> a -> Msg
errHelper descMsg errMsg =
    let
        errStr =
            toString errMsg
    in
    LogErr (Debug.log errStr <| descMsg ++ errStr)


handleDelegationPayload r =
    case r of
        Ok payload ->
            ReceivedPayload payload

        Err err ->
            errHelper "Delegation Payload error: " err


handleContractRead : Model -> ReadResponseWCarry -> Msg
handleContractRead model r =
    let
        respHs =
            [ ( model.delegationAddr, handleDelegateReadResp ) ]

        default _ =
            LogErr (Debug.log "DelegateUI: Unknown contract read response: " <| toString r)
    in
    case List.head <| List.filter (\( addr_, h ) -> Debug.log ("filtering: " ++ addr_ ++ "," ++ r.addr) <| addr_ == r.addr) respHs of
        Just ( addr, h ) ->
            Debug.log "calling handler" <| h r

        Nothing ->
            default ()


encodeDlgtResp : DelegationResp -> Value
encodeDlgtResp r =
    E.object
        [ ( "delegationId", E.int r.dId )
        , ( "prevDelegationId", E.int r.prevDId )
        , ( "setAtBlock", E.int r.setAtBlock )
        , ( "delegate", E.string r.delegatee )
        , ( "voter", E.string r.delegator )
        , ( "tokenContract", E.string r.tokenAddr )
        ]


handleDelegateReadResp r =
    let
        resToRemData =
            Result.Extra.unpack Failure Success

        -- reportGotDelegator =
        --     msgOrErr GotDlgtVoterCheck LogErr
    in
    if r.success then
        case r.method of
            "resolveDelegation" ->
                decodeValue decDelegationResp r.response
                    |> resToRemData
                    >> ViewDlgtResp

            -- "findPossibleDelegatorsOf" ->
            --     msgOrErr CheckDlgtsForVoter LogErr <| decodeValue decDelegatorsOfResp r.response
            _ ->
                let
                    errMsg =
                        "DelegateUI: unknown method on Delegation SC: " ++ r.method
                in
                ViewDlgtResp (Failure errMsg)
    else
        ViewDlgtResp (Failure r.errMsg)
