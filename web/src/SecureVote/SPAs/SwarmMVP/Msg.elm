module SecureVote.SPAs.SwarmMVP.Msg exposing (..)

import Material
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route)


type Msg
    = SetElevation Int Bool
    | SetField String String
    | ChangePage Route
      -- Elm Mdl
    | Mdl (Material.Msg Msg)
