module SecureVote.SPAs.SwarmMVP.Model exposing (..)

import Dict exposing (Dict)
import Material
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmAddressR))


type alias Model =
    { swmAddress : Maybe String
    , count : Int
    , mdl : Material.Model
    , elevations : Dict Int Bool
    , fields : Dict String String
    , route : Route
    , history : List Route
    }


initModel : Model
initModel =
    { swmAddress = Nothing
    , count = 0
    , mdl = Material.model
    , elevations = Dict.empty
    , fields = Dict.empty
    , route = SwmAddressR
    , history = []
    }
