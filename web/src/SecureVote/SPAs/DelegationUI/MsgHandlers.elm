module SecureVote.SPAs.DelegationUI.MsgHandlers exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as E
import RemoteData exposing (RemoteData(..))
import Result.Extra
import SecureVote.Eth.Types exposing (ReadResponse)
import SecureVote.SPAs.DelegationUI.Model exposing (Model)
import SecureVote.SPAs.DelegationUI.Msg exposing (..)
import SecureVote.SPAs.DelegationUI.Types exposing (..)


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


handleContractRead : Model -> ReadResponse -> Msg
handleContractRead model r =
    let
        respHs =
            [ ( model.delegationAddr, handleDelegateReadResp ) ]

        default =
            LogErr (Debug.log "DelegateUI: Unknown contract read response: " <| toString r)
    in
    case List.head <| List.filter (\( addr_, h ) -> Debug.log ("filtering: " ++ addr_ ++ "," ++ r.addr) <| addr_ == r.addr) respHs of
        Just ( addr, h ) ->
            Debug.log "calling handler" <| h r

        Nothing ->
            default


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
        reqIx i dec decoder =
            custom (index i dec) decoder

        myInt =
            string |> andThen (decodeString int >> Result.Extra.unpack fail succeed)

        d =
            decode DelegationResp
                |> reqIx 0 myInt
                |> reqIx 1 myInt
                |> reqIx 2 myInt
                |> reqIx 3 string
                |> reqIx 4 string
                |> reqIx 5 string
    in
    case r.method of
        "resolveDelegation" ->
            ViewDlgtResp <| Result.Extra.unpack Failure Success <| decodeValue d r.response

        _ ->
            let
                errMsg =
                    "DelegateUI: unknown method on Delegation SC: " ++ r.method
            in
            ViewDlgtResp (Failure errMsg)
