module SecureVote.Components.UI.Elevation exposing (..)

import Dict
import Material.Elevation as Elevation
import Material.Options as Options
import Maybe.Extra exposing ((?))
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(SetElevation))


elevation : Int -> Model -> List (Options.Property a Msg)
elevation id model =
    let
        elevation id =
            case Dict.get id model.elevations ? False of
                False ->
                    Elevation.e2

                True ->
                    Elevation.e6
    in
    [ elevation id
    , Options.onMouseOver (SetElevation id True)

    --    , Options.onMouseDown (SetElevation id True)
    , Options.onMouseLeave (SetElevation id False)

    --    , Options.onMouseUp (SetElevation id )
    , Elevation.transition 125
    ]
