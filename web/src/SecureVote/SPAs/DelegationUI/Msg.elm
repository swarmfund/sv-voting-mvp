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
    | GotDelegationPayload String
    | FromWeb3 FromWeb3Msg
    | LogErr String
    | ToWeb3 ToWeb3Msg


type FromWeb3Msg
    = GotTxid String


type ToWeb3Msg
    = SendTxToMM MinEthTx
