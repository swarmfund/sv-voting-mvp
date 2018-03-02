module SecureVote.SPAs.AdminUI.Msg exposing (..)

import Element.Input exposing (SelectMsg)
import SecureVote.Ballots.Types exposing (BallotSpecChoice, OptsChoice)
import SecureVote.Crypto.Hashing exposing (HashAlg, HashMsg)
import SecureVote.Eth.Models exposing (MinEthTx)
import SecureVote.Eth.Types exposing (WriteViaMMDoc)


type Msg
    = Nop
    | SetStrField String String
    | SelectBallot (SelectMsg BallotSpecChoice)
    | SelectOptType (SelectMsg OptsChoice)
    | SetBoolField String Bool
    | UpdateWrkBallot
    | SaveJson
    | MMsg (List Msg)
    | HashError String
    | UpdateHash HashMsg
    | LogErr String
    | ToWeb3 ToWeb3Msg
    | FromWeb3 FromWeb3Msg


type FromWeb3Msg
    = GotTxid String


type ToWeb3Msg
    = SendTxToMM MinEthTx
    | WriteViaMM WriteViaMMDoc
