module SecureVote.SPAs.AdminUI.Msg exposing (..)

import Element.Input exposing (SelectMsg)
import Json.Encode exposing (Value)
import RemoteData exposing (RemoteData)
import SecureVote.Ballots.Types exposing (BallotSpecChoice, OptsChoice)
import SecureVote.Crypto.Hashing exposing (HashAlg, HashMsg)
import SecureVote.Eth.Types exposing (..)


type Msg
    = Nop
    | SetStrField String String
    | SetIntField String Int
    | SelectBallot (SelectMsg BallotSpecChoice)
    | SelectOptType (SelectMsg OptsChoice)
    | SetBoolField String Bool
    | ToggleBoolField String
    | SetLoadingField String (RemoteData String String)
    | ResetBallotPubFields String
    | UpdateWrkBallot
    | SaveJson
    | MMsg (List Msg)
    | HashError String
    | UpdateHash HashMsg
    | LogErr String
    | ToWeb3 ToWeb3Msg
    | FromWeb3 FromWeb3Msg
    | UploadBallot { andThen : Msg }


type FromWeb3Msg
    = GotTxid String
    | GotTxInfo String


type ToWeb3Msg
    = SendTxToMM MinEthTx
    | WriteViaMM WriteViaMMDoc
