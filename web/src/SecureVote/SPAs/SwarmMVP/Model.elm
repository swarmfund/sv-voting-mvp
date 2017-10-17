module SecureVote.SPAs.SwarmMVP.Model exposing (..)

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Html exposing (Html, div)
import Material
import Material.Snackbar
import SecureVote.Eth.Models exposing (CandidateEthTx, nullCandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmAddressR))


type alias Model =
    { mdl : Material.Model
    , snack : Material.Snackbar.Model Msg
    , errors : List String
    , dialogHtml : Html Msg
    , elevations : Dict Int Bool
    , fields : Dict String String
    , ballotRange : Dict Int Int
    , route : Route
    , history : List Route
    , candidateTx : CandidateEthTx
    , ethNode : String
    , swarmErc20Address : String
    , swmBalance : Maybe Decimal
    }


initModel : Model
initModel =
    { mdl = Material.model
    , snack = Material.Snackbar.model
    , errors = []
    , dialogHtml = div [] []
    , elevations = Dict.empty
    , fields = Dict.empty
    , ballotRange = Dict.empty
    , route = SwmAddressR
    , history = []
    , candidateTx = { nullCandidateEthTx | to = Just "SWARM VOTING SMART CONTRACT ADDRESS" }
    , ethNode = "https://mainnet.infura.io"

    -- golem address
    , swarmErc20Address = "0xa74476443119A942dE498590Fe1f2454d7D4aC0d"
    , swmBalance = Nothing
    }
