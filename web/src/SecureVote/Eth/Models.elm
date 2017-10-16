module SecureVote.Eth.Models exposing (..)


type alias CandidateEthTx =
    { from : Maybe String
    , value : Int
    , to : Maybe String
    , data : Maybe String
    }


nullCandidateEthTx =
    CandidateEthTx Nothing 0 Nothing Nothing


type alias MinEthTx =
    { from : String
    , to : String
    , data : String
    , value : String
    }
