module SecureVote.SPAs.DelegationUI.Msg exposing (..)

import Element.Input exposing (SelectMsg)
import SecureVote.Ballots.Types exposing (BallotSpecChoice, OptsChoice)
import SecureVote.Eth.Models exposing (MinEthTx)


type Msg
    = Nop
    | SetStrField String String
    | SelectBallot (SelectMsg BallotSpecChoice)
    | SelectOptType (SelectMsg OptsChoice)
    | SetBoolField String Bool
    | UpdateWrkBallot
    | SaveJson
    | MMsg (List Msg)
    | Sha3Error String
    | UpdateSha3 String
    | FromWeb3 FromWeb3Msg
    | LogErr String
    | ToWeb3 ToWeb3Msg


type FromWeb3Msg
    = GotTxid String


type ToWeb3Msg
    = SendTxToMM MinEthTx