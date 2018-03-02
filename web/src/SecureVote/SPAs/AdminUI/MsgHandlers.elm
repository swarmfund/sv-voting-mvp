module SecureVote.SPAs.AdminUI.MsgHandlers exposing (..)

import SecureVote.SPAs.AdminUI.Msg exposing (FromWeb3Msg(GotTxid), Msg(..))


errHelper : String -> a -> Msg
errHelper descMsg errMsg =
    let
        errStr =
            toString errMsg
    in
    MMsg [ LogErr (Debug.log errStr <| descMsg ++ errStr) ]


handleMetaMaskTxid r =
    case r of
        Ok txid ->
            MMsg [ FromWeb3 <| GotTxid txid, LogErr <| "TXID from MetaMask: " ++ txid ]

        Err err ->
            errHelper "MetaMask error: " err
