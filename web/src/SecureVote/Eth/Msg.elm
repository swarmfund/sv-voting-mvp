module SecureVote.Eth.Msg exposing (..)

import Json.Decode exposing (Value)
import SecureVote.Eth.Types exposing (..)


type EthMsg
    = EthNop
    | WriteViaMM WriteViaMMDoc
    | ReadContract ReadContractWCarryDoc
    | RefreshMMAddress
    | SetMMAddress String
    | SetEthProvider String
    | GotWeb3 Value
