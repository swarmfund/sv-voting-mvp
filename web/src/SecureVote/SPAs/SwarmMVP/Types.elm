module SecureVote.SPAs.SwarmMVP.Types exposing (..)


type TxidCheckStatus
    = TxidNotMade
    | TxidInProgress
    | TxidSuccess
    | TxidFail String


type alias GotTxidResp =
    { data : String
    , confirmed : Bool
    }
