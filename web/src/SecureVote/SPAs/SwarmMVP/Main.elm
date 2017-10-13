module SecureVote.SPAs.SwarmMVP.Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Update exposing (update)
import SecureVote.SPAs.SwarmMVP.Views.RootV exposing (rootView)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []


initCmds : Cmd Msg
initCmds =
    Cmd.batch
        []


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, initCmds )
        , view = rootView
        , subscriptions = always Sub.none
        , update = update
        }
