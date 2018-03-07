module SecureVote.SPAs.DelegationUI.Types exposing (..)


type alias Flags =
    { mainTitle : String, dev : Bool, democHash : String, delegationABI : String, delegationAddr : String }


type alias DelegationTx =
    { to : String
    , from : String
    , value : Int
    , data : String
    }


type DelegationType
    = Global
    | Token
