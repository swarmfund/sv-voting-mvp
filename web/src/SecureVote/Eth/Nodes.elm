module SecureVote.Eth.Nodes exposing (..)

--import SecureVote.Ballots.LittleGovIndexes exposing (NetworkTo)


type alias NetworkTo a =
    { kovan : a, mainnet : a, ropsten : a }


ethNodes : NetworkTo String
ethNodes =
    { mainnet = "https://mainnet.eth.secure.vote:8545/svLight"
    , kovan = "https://kovan.eth.secure.vote:8545/svLightDev"
    , ropsten = "https://ropsten.infura.io/someApiKeyShrug"
    }
