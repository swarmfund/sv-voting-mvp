module SecureVote.Eth.Msg exposing (..)

import SecureVote.Eth.Types exposing (..)


type EthMsg
    = WriteViaMM WriteViaMMDoc
    | ReadContract ReadContractDoc
    | RefreshMMAddress
    | SetMMAddress String
