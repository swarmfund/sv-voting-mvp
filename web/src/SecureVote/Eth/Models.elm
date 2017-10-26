module SecureVote.Eth.Models exposing (..)


type alias CandidateEthTx =
    { from : Maybe String
    , to : Maybe String
    , value : Int
    , data : Maybe String
    , gas : String
    }


nullCandidateEthTx : CandidateEthTx
nullCandidateEthTx =
    -- Gas set to 200,000
    { from = Nothing, to = Nothing, value = 0, data = Just "", gas = "0x030d40" }


type alias MinEthTx =
    { from : String
    , to : String
    , value : Int
    , data : String
    , gas : String
    }
