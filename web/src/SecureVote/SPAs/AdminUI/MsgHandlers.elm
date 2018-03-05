module SecureVote.SPAs.AdminUI.MsgHandlers exposing (..)

import SecureVote.SPAs.AdminUI.Msg exposing (FromWeb3Msg(..), Msg(..))


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


handleGotTxInfoAdmin : Result String String -> Msg
handleGotTxInfoAdmin r =
    case r of
        Ok gotTxInfo ->
            FromWeb3 <| GotTxInfo gotTxInfo

        Err err ->
            errHelper "GotTxInfo error generating tx data: " err
