module SecureVote.Eth.Model exposing (..)

import SecureVote.Eth.Types exposing (..)


type alias EthFlags =
    { mmDetected : Bool, ethNode : String }


type alias EthMdl =
    { pendingTxs : List TxidLog
    , pastTxs : List TxidLog
    , mmAddr : Maybe String
    , mmDetected : Bool
    , ethNode : String
    }


initEthMdl : EthFlags -> EthMdl
initEthMdl { mmDetected, ethNode } =
    { pendingTxs = []
    , pastTxs = []
    , mmAddr = Nothing
    , mmDetected = mmDetected
    , ethNode = ethNode
    }
