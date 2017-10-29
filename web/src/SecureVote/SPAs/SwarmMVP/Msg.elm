module SecureVote.SPAs.SwarmMVP.Msg exposing (..)

import Decimal exposing (Decimal)
import Material
import Material.Snackbar as Snackbar
import RemoteData exposing (RemoteData)
import SecureVote.Crypto.Curve25519 exposing (Curve25519KeyPair)
import SecureVote.Eth.Models exposing (CandidateEthTx)
import SecureVote.Eth.Types exposing (InitRecord)
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute, Route)
import SecureVote.SPAs.SwarmMVP.Types exposing (GotTxidResp)
import Time exposing (Time)


type Msg
    = NoOp
    | SetTime Int
      -- ** UI Maintenance msgs
    | SetElevation Int Bool
    | SetField String String
    | PageGoForward Route
    | PageGoBack
    | SetDialog String (DialogRoute Msg)
      -- ** Voting msgs
    | SetBallotRange Int Float
    | ConstructBallotPlaintext
      -- ** Eth related msgs
    | SetCandidateTx (CandidateEthTx -> CandidateEthTx)
    | SetEthNode String
      -- ** Utility msgs
    | MultiMsg (List Msg)
      -- ** Port msgs
    | ToWeb3 ToWeb3Msg
    | FromWeb3 FromWeb3Msg
    | FromCurve25519 FromCurve25519Msg
      -- | ToCurve25519 ToCurve25519Msg
      -- ** Errors
    | LogErr String
    | Snackbar (Snackbar.Msg String)
      -- ** Elm Mdl
    | Mdl (Material.Msg Msg)


type ToWeb3Msg
    = SetProvider
    | GetErc20Balance
    | CheckTxid String


type FromWeb3Msg
    = GotBalance Decimal
    | GotDataParam String
    | GotEncPubkey String
    | Web3Init InitRecord
    | GetBallotOpts (RemoteData String (List (List Int)))
    | GotTxidStatus (Result String GotTxidResp)
    | GetBallotPeriod (RemoteData String { startTime : Int, endTime : Int })


type FromCurve25519Msg
    = GotKey Curve25519KeyPair
    | GotEncBytes String


type ToCurve25519Msg
    = GenerateSignedBallot
