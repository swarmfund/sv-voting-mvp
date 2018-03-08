module SecureVote.Eth.Model exposing (..)

import SecureVote.Eth.Types exposing (..)


type alias EthMdl =
    { pendingTxs : List TxidLog
    , pastTxs : List TxidLog
    }


initEthMdl : EthMdl
initEthMdl =
    { pendingTxs = []
    , pastTxs = []
    }
