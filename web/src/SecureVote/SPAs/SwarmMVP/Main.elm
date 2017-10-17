module SecureVote.SPAs.SwarmMVP.Main exposing (..)

import Html exposing (..)
import SecureVote.Eth.Web3 exposing (setWeb3Provider)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Update exposing (update)
import SecureVote.SPAs.SwarmMVP.Views.RootV exposing (rootView)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []


initCmds : Model -> Cmd Msg
initCmds initModel =
    Cmd.batch
        [ setWeb3Provider initModel.ethNode ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, initCmds initModel )
        , view = rootView
        , subscriptions = always Sub.none
        , update = update
        }
