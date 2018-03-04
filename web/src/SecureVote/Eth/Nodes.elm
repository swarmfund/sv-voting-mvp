module SecureVote.Eth.Nodes exposing (..)

--import SecureVote.Ballots.LittleGovIndexes exposing (NetworkTo)


type alias NetworkTo a =
    { kovan : a, mainnet : a, ropsten : a }


ethNodes : NetworkTo String
ethNodes =
    { mainnet = "https://eth-aws-nv-node-02.secure.vote:8545/svLight"
    , kovan = "https://eth-kovan-aws-nv-node-01.secure.vote:8545/svLightDev"
    , ropsten = "https://ropsten.infura.io/someApiKeyShrug"
    }
