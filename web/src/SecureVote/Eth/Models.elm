module SecureVote.Eth.Models exposing (..)


type alias CandidateEthTx =
    { from : Maybe String
    , value : Int
    , to : Maybe String
    , data : Maybe String
    }


setCandTxFrom : String -> CandidateEthTx -> CandidateEthTx
setCandTxFrom from tx =
    { tx | from = Just from }


nullCandidateEthTx =
    CandidateEthTx Nothing 0 Nothing Nothing
