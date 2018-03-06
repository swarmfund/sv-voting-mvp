module SecureVote.SPAs.SwarmMVP.Types exposing (..)


type TxidCheckStatus
    = TxidNotMade
    | TxidInProgress
    | TxidSuccess
    | TxidFail String


type alias GotTxidResp =
    { data : String
    , confirmed : Bool
    , gas : Int
    , logMsg : String
    }


type alias Flags =
    { mainTitle : String
    , dev : Bool
    , democHash : String
    , indexABI : String
    , indexAddr : String
    , ballotBoxABI : String --^ BallotBox ABI
    , delegationABI : String
    , delegationAddr : String
    }
