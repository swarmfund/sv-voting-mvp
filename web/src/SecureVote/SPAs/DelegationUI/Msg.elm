module SecureVote.SPAs.DelegationUI.Msg exposing (..)

import Element.Input exposing (SelectMsg)
import SecureVote.Eth.Models exposing (MinEthTx)
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationType, TokenContractChoice)


type Msg
    = Nop
    | SetStrField String String
    | SelectTokenContract (SelectMsg TokenContractChoice)
    | SetDelegationType DelegationType
    | SetBoolField String Bool
    | GetDelegationPayload { delegateAddr : String, tokenAddr : String }
    | ReceivedPayload String
    | LogErr String
