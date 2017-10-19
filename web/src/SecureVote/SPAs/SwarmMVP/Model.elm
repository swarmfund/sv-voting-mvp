module SecureVote.SPAs.SwarmMVP.Model exposing (..)

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Material
import Material.Snackbar
import SecureVote.Crypto.Curve25519 exposing (ReceiveKeyPair)
import SecureVote.Eth.Models exposing (CandidateEthTx, nullCandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(NotFoundDialog), Route(NotFoundR, SwmAddressR))


type alias Model =
    { mdl : Material.Model
    , snack : Material.Snackbar.Model Msg
    , errors : List String
    , dialogHtml : { title : String, route : DialogRoute Msg }
    , elevations : Dict Int Bool
    , fields : Dict String String
    , ballotRange : Dict Int Int
    , ballotBits : Dict Int (Result String (List Int))
    , route : Route
    , history : List Route
    , lastPageDirection : LastPageDirection
    , lastRoute : Maybe Route
    , candidateTx : CandidateEthTx
    , ethNode : String
    , swarmErc20Address : String
    , swmBalance : Maybe Decimal
    , keypair : Maybe ReceiveKeyPair
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
    }


type LastPageDirection
    = PageForward
    | PageBack
