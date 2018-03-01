module SecureVote.SPAs.AdminUI.Main exposing (..)

import Html
import SecureVote.Eth.Web3 exposing (metamaskTxidGen)
import SecureVote.SPAs.AdminUI.Model exposing (Model, initModel)
import SecureVote.SPAs.AdminUI.Msg exposing (Msg)
import SecureVote.SPAs.AdminUI.MsgHandlers exposing (handleMetaMaskTxid)
import SecureVote.SPAs.AdminUI.Update exposing (update)
import SecureVote.SPAs.AdminUI.Views.RootV exposing (rootV)


type alias Flags =
    { mainTitle : String, dev : Bool, democHash : String }


initF : Flags -> ( Model, Cmd Msg )
initF f =
    initModel f.mainTitle f.dev f.democHash ! []


subscriptions model =
    Sub.batch
        [ metamaskTxidGen handleMetaMaskTxid ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = initF
        , view = rootV
        , subscriptions = subscriptions
        , update = update
        }
