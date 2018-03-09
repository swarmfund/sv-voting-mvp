module SecureVote.Eth.Model exposing (..)

import SecureVote.Eth.Types exposing (..)


type alias EthFlags =
    { mmDetected : Bool }


type alias EthMdl =
    { pendingTxs : List TxidLog
    , pastTxs : List TxidLog
    , mmAddr : Maybe String
    , mmDetected : Bool
    }


initEthMdl : EthFlags -> EthMdl
initEthMdl { mmDetected } =
    { pendingTxs = []
    , pastTxs = []
    , mmAddr = Nothing
    , mmDetected = mmDetected
    }
