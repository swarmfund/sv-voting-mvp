module SecureVote.SPAs.DelegationUI.Msg exposing (..)

import Element.Input exposing (SelectMsg)
import RemoteData exposing (RemoteData)
import SecureVote.Eth.Msg as EthMsg
import SecureVote.Eth.Types exposing (MinEthTx)
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationResp, DelegationType)
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
    | ViewDlgtResp (RemoteData String DelegationResp)
    | MMsg (List Msg)
      -- Web3 calls
    | Web3 EthMsg.Msg
