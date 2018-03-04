module SecureVote.SPAs.DelegationUI.MsgHandlers exposing (..)

import SecureVote.SPAs.DelegationUI.Msg exposing (..)


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
            GotDelegationPayload payload

        Err err ->
            errHelper "Delegation Payload error: " err
