module SecureVote.SPAs.DelegationUI.Types exposing (..)


type alias Flags =
    { mainTitle : String, dev : Bool, democHash : String, indexAddr : String, indexABI : String, delegationABI : String }


type alias DelegationTx =
    { to : String
    , from : String
    , value : Int
    , gas : Int
    , data : String
    }
