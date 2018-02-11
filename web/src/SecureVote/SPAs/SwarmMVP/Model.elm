module SecureVote.SPAs.SwarmMVP.Model exposing (..)

import Dict exposing (Dict)
import List exposing (foldl, map)
import Material
import Material.Snackbar
import RemoteData exposing (RemoteData(..))
import SecureVote.Crypto.Curve25519 exposing (Curve25519KeyPair)
import SecureVote.Eth.Models exposing (CandidateEthTx, nullCandidateEthTx)
import SecureVote.Eth.Types exposing (AuditDoc)
import SecureVote.SPAs.SwarmMVP.Ballot exposing (allBallots)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(NotFoundDialog), Route(ListAllVotesR, NotFoundR, SwmAddressR))
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
    , allBallots : Dict Int (BallotParams Msg)
    , route : Route
    , history : List Route
    , lastPageDirection : LastPageDirection
    , lastRoute : Maybe Route
    , candidateTx : CandidateEthTx
    , ethNode : String
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
    , mainTitle : String
    , auditMsgs : List AuditDoc
    }


initModel : BallotParams Msg -> String -> Model
initModel defaultBallot mainTitle =
    { mdl = Material.model
    , snack = Material.Snackbar.model
    , errors = []
    , dialogHtml = { title = "Error: Dialog has not been updated.", route = NotFoundDialog }
    , elevations = Dict.empty
    , fields = Dict.empty
    , ballotRange = Dict.empty
    , ballotBits = Dict.empty
    , ballotAllDone = False
    , currentBallot = defaultBallot
    , allBallots = foldl (\b m -> Dict.insert b.id b m) Dict.empty allBallots
    , route = ListAllVotesR
    , history = []
    , lastRoute = Nothing
    , lastPageDirection = PageForward
    , candidateTx = nullCandidateEthTx
    , ethNode = initEthNode

    --    , ethNode = "http://localhost:8545"
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
    , mainTitle = mainTitle
    , auditMsgs = []
    }


initEthNode : String
initEthNode =
    "http://eth-aws-nv-node-02.secure.vote:38545/eth"



--    "https://mainnet.infura.io/securevote"


type LastPageDirection
    = PageForward
    | PageBack


resetAllBallotFields : Model -> BallotParams Msg -> Model
resetAllBallotFields model { contractAddr } =
    { model
        | ballotRange = Dict.empty
        , ballotBits = Dict.empty
        , ballotAllDone = False
        , candidateTx = { nullCandidateEthTx | to = Just contractAddr }
        , encBytes = Nothing
        , ballotPlaintext = Nothing
        , remoteHexPk = Nothing
        , miniVotingAbi = "Error: Ballot parameters have been reset and ABI is not set yet."
        , ballotVerificationPassed = Loading
        , txidCheck = TxidNotMade
        , ballotOpen = Loading
        , auditMsgs = []
    }
