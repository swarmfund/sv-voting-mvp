module SecureVote.SPAs.SwarmMVP.Model exposing (..)

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Material
import Material.Snackbar
import SecureVote.Crypto.Curve25519 exposing (Curve25519KeyPair)
import SecureVote.Eth.Models exposing (CandidateEthTx, nullCandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Const exposing (erc20Addr, votingContractAddr)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(NotFoundDialog), Route(NotFoundR, SwmAddressR))
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
    , route : Route
    , history : List Route
    , lastPageDirection : LastPageDirection
    , lastRoute : Maybe Route
    , candidateTx : CandidateEthTx
    , ethNode : String
    , swarmErc20Address : String
    , swarmVotingAddress : String
    , swmBalance : Maybe Decimal
    , keypair : Maybe Curve25519KeyPair
    , encBytes : Maybe String
    , ballotPlaintext : Maybe (List Int)
    , remoteHexPk : Maybe String
    , miniVotingAbi : String
    }


initModel : Model
initModel =
    { mdl = Material.model
    , snack = Material.Snackbar.model
    , errors = []
    , dialogHtml = { title = "", route = NotFoundDialog }
    , elevations = Dict.empty
    , fields = Dict.empty
    , ballotRange = Dict.empty
    , ballotBits = Dict.empty
    , ballotAllDone = False
    , route = SwmAddressR
    , history = []
    , lastRoute = Nothing
    , lastPageDirection = PageForward
    , candidateTx = { nullCandidateEthTx | to = Just votingContractAddr }
    , ethNode = "https://mainnet.infura.io"
    , swarmErc20Address = erc20Addr
    , swarmVotingAddress = votingContractAddr
    , swmBalance = Nothing
    , keypair = Nothing
    , encBytes = Nothing
    , ballotPlaintext = Nothing
    , remoteHexPk = Nothing
    , miniVotingAbi = "Error: Web3 has not initialized correctly"
    }


type LastPageDirection
    = PageForward
    | PageBack
