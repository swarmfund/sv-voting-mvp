module SecureVote.SPAs.DelegationUI.Types exposing (..)


type alias Flags =
    { mainTitle : String, dev : Bool, democHash : String, indexAddr : String, indexABI : String, ballotBoxABI : String }
