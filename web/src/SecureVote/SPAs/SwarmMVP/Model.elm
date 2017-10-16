module SecureVote.SPAs.SwarmMVP.Model exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Material
import SecureVote.Eth.Models exposing (CandidateEthTx, nullCandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmAddressR))


type alias Model =
    { mdl : Material.Model
    , dialogHtml : Html Msg
    , elevations : Dict Int Bool
    , fields : Dict String String
    , ballotRange : Dict Int Int
    , route : Route
    , history : List Route
    , candidateTx : CandidateEthTx
    , ethNode : String
    }


initModel : Model
initModel =
    { mdl = Material.model
    , dialogHtml = div [] []
    , elevations = Dict.empty
    , fields = Dict.empty
    , ballotRange = Dict.empty
    , route = SwmAddressR
    , history = []
    , candidateTx = { nullCandidateEthTx | to = Just "SWARM VOTING SMART CONTRACT ADDRESS" }
    , ethNode = "https://mainnet.infura.io"
    }
