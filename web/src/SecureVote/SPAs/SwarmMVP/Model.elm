module SecureVote.SPAs.SwarmMVP.Model exposing (..)

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Material
import Material.Snackbar
import SecureVote.Crypto.Curve25519 exposing (Curve25519KeyPair)
import SecureVote.Eth.Models exposing (CandidateEthTx, nullCandidateEthTx)
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
    , swmBalance : Maybe Decimal
    , keypair : Maybe Curve25519KeyPair
    , encBytes : Maybe (List Int)
    , ballotPlaintext : Maybe (List Int)
    , remoteHexPk : Maybe String
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
    , candidateTx = { nullCandidateEthTx | to = Just "SWARM VOTING SMART CONTRACT ADDRESS" }
    , ethNode = "https://mainnet.infura.io"

    -- golem address
    , swarmErc20Address = "0xa74476443119A942dE498590Fe1f2454d7D4aC0d"
    , swmBalance = Nothing
    , keypair = Nothing
    , encBytes = Nothing
    , ballotPlaintext = Nothing
    , remoteHexPk = Just "ef524153a1a69d4a1644fabf34177bd24caa7ae09909574d25098c9f376e123f"
    }


type LastPageDirection
    = PageForward
    | PageBack
