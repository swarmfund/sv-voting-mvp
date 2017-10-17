module SecureVote.SPAs.SwarmMVP.Main exposing (..)

import Html exposing (..)
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
