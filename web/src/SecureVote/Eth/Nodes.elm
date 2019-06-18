module SecureVote.Eth.Nodes exposing (..)

--import SecureVote.Ballots.LittleGovIndexes exposing (NetworkTo)


type alias NetworkTo a =
    { kovan : a, mainnet : a, ropsten : a }


ethNodes : NetworkTo String
ethNodes =
    { mainnet = "https://mainnet.eth.secure.vote:8545/svLight"
    , kovan = "kovan.infura.io/v3/9f8da3240536480e94a63f74769f9da6"
    , ropsten = "https://ropsten.infura.io/someApiKeyShrug"
    }
