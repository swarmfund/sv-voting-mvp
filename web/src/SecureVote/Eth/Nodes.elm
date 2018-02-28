module SecureVote.Eth.Nodes exposing (..)

import SecureVote.Ballots.LittleGovIndexes exposing (NetworkTo)


ethNodes : NetworkTo String
ethNodes =
    { mainnet = "https://eth-aws-nv-node-02.secure.vote:8545/littleGov"
    , kovan = "https://eth-kovan-aws-nv-node-01.secure.vote:8545/littleGovDev"
    , ropsten = "https://ropsten.infura.io/someApiKeyShrug"
    }
