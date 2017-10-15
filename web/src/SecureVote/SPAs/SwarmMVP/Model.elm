module SecureVote.SPAs.SwarmMVP.Model exposing (..)

import Dict exposing (Dict)
import Material
import SecureVote.Eth.Models exposing (CandidateEthTx, nullCandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmAddressR))


type alias Model =
    { mdl : Material.Model
    , elevations : Dict Int Bool
    , fields : Dict String String
    , route : Route
    , history : List Route
    , candidateTx : CandidateEthTx
    }


initModel : Model
initModel =
    { mdl = Material.model
    , elevations = Dict.empty
    , fields = Dict.empty
    , route = SwmAddressR
    , history = []
    , candidateTx = nullCandidateEthTx
    }
