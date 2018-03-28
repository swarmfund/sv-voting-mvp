module SecureVote.SPAs.AdminUI.Types exposing (..)


type alias Flags =
    { mainTitle : String
    , dev : Bool
    , democHash : String
    , indexAddr : String
    , indexABI : String
    , ballotBoxABI : String
    , archivePushApiKey : String
    }
