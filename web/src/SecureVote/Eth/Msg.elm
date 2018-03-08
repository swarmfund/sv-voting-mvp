module SecureVote.Eth.Msg exposing (..)

import SecureVote.Eth.Types exposing (..)


type Msg
    = WriteViaMM WriteViaMMDoc
    | ReadContract ReadContractDoc
