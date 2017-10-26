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
    , encBytes : Maybe String
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
    , candidateTx = { nullCandidateEthTx | to = Just "0x76a2ddedc76ab65eda35955e8f1bb5569675238c" }
    , ethNode = "http://localhost:8545"

    -- golem address
    , swarmErc20Address = "0x1bf9dd536d4f95eae4ad79f96b7e495050c917c3"
    , swmBalance = Nothing
    , keypair = Nothing
    , encBytes = Nothing
    , ballotPlaintext = Nothing
    , remoteHexPk = Just "c11348fc9c94b761b2b34345b68280672738655b2ce45d6b9b038797d015f801"
    }


type LastPageDirection
    = PageForward
    | PageBack
