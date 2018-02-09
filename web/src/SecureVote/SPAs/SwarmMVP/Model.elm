module SecureVote.SPAs.SwarmMVP.Model exposing (..)

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Material
import Material.Snackbar
import RemoteData exposing (RemoteData(..))
import SecureVote.Crypto.Curve25519 exposing (Curve25519KeyPair)
import SecureVote.Eth.Models exposing (CandidateEthTx, nullCandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Ballot exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Const exposing (erc20Addr)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(NotFoundDialog), Route(NotFoundR, OpeningSlideR, SwmAddressR))
import SecureVote.SPAs.SwarmMVP.Types exposing (TxidCheckStatus(TxidNotMade))
import SecureVote.Voting.Types.RangeVoting exposing (RangeBallot3Bits)


type alias Model =
    { mdl : Material.Model
    , snack : Material.Snackbar.Model String
    , errors : List String
    , dialogHtml : { title : String, route : DialogRoute Msg }
    , elevations : Dict Int Bool
    , fields : Dict String String
    , ballotRange : Dict Int Int
    , ballotBits : Dict Int (Result String RangeBallot3Bits)
    , ballotAllDone : Bool
    , currentBallot : BallotParams Msg
    , route : Route
    , history : List Route
    , lastPageDirection : LastPageDirection
    , lastRoute : Maybe Route
    , candidateTx : CandidateEthTx
    , ethNode : String
    , swarmErc20Address : String
    , swmBalance : Maybe Decimal
    , keypair : Maybe Curve25519KeyPair
    , encBytes : Maybe String
    , ballotPlaintext : Maybe (List Int)
    , remoteHexPk : Maybe String
    , miniVotingAbi : String
    , verificationError : Maybe String
    , ballotVerificationPassed : RemoteData String Bool
    , txidCheck : TxidCheckStatus
    , ballotOpen : RemoteData String { startTime : Int, endTime : Int }
    , now : Int
    }


initModel : BallotParams Msg -> Model
initModel defaultBallot =
    { mdl = Material.model
    , snack = Material.Snackbar.model
    , errors = []
    , dialogHtml = { title = "", route = NotFoundDialog }
    , elevations = Dict.empty
    , fields = Dict.empty
    , ballotRange = Dict.empty
    , ballotBits = Dict.empty
    , ballotAllDone = False
    , currentBallot = defaultBallot
    , route = OpeningSlideR
    , history = []
    , lastRoute = Nothing
    , lastPageDirection = PageForward
    , candidateTx = nullCandidateEthTx
    , ethNode = initEthNode

    --    , ethNode = "http://localhost:8545"
    , swarmErc20Address = erc20Addr
    , swmBalance = Nothing
    , keypair = Nothing
    , encBytes = Nothing
    , ballotPlaintext = Nothing
    , remoteHexPk = Nothing
    , miniVotingAbi = "Error: Web3 has not initialized correctly"
    , verificationError = Just "Hi"
    , ballotVerificationPassed = Loading
    , txidCheck = TxidNotMade
    , ballotOpen = Loading
    , now = 0
    }


initEthNode : String
initEthNode =
    "https://mainnet.infura.io/securevote"


type LastPageDirection
    = PageForward
    | PageBack
