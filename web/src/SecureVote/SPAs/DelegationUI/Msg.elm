module SecureVote.SPAs.DelegationUI.Msg exposing (..)

import Element.Input exposing (SelectMsg)
import SecureVote.Eth.Models exposing (MinEthTx)
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationType)
import SecureVote.Tokens.Types exposing (TokenContract)


type Msg
    = Nop
    | SetStrField String String
    | SelectTokenContract (SelectMsg TokenContract)
    | SetDelegationType DelegationType
    | SetBoolField String Bool
    | ToggleBoolField String
    | OnFieldUpdate
    | ReceivedPayload String
    | LogErr String
