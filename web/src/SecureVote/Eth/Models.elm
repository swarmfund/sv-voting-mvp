module SecureVote.Eth.Models exposing (..)


type alias CandidateEthTx =
    { from : Maybe String
    , value : Int
    , to : Maybe String
    , data : Maybe String
    , gas : String
    }


nullCandidateEthTx : CandidateEthTx
nullCandidateEthTx =
    -- Gas set to 200,000
    CandidateEthTx Nothing 0 Nothing (Just "") "0x030d40"


type alias MinEthTx =
    { from : String
    , to : String
    , data : String
    , value : String
    , gas : String
    }
