module SecureVote.SPAs.AdminUI.MsgHandlers exposing (..)

import SecureVote.SPAs.AdminUI.Msg exposing (FromWeb3Msg(GotTxid), Msg(..))


errHelper : String -> a -> Msg
errHelper descMsg errMsg =
    let
        errStr =
            toString errMsg
    in
    LogErr (Debug.log errStr <| descMsg ++ errStr)


handleMetaMaskTxid r =
    case r of
        Ok txid ->
            FromWeb3 <| GotTxid txid

        Err err ->
            errHelper "MetaMask error: " err
