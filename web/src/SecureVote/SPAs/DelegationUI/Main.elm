module SecureVote.SPAs.DelegationUI.Main exposing (..)

import Html
import SecureVote.Eth.Web3 exposing (gotDelegatePayloadGen, metamaskTxidGen)
import SecureVote.SPAs.DelegationUI.Model exposing (Model, initModel)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg)
import SecureVote.SPAs.DelegationUI.MsgHandlers exposing (handleDelegationPayload)
import SecureVote.SPAs.DelegationUI.Types exposing (Flags)
import SecureVote.SPAs.DelegationUI.Update exposing (update)
import SecureVote.SPAs.DelegationUI.Views.RootV exposing (rootV)


initF : Flags -> ( Model, Cmd Msg )
initF f =
    initModel f ! []


subscriptions model =
    Sub.batch
        [ gotDelegatePayloadGen handleDelegationPayload
        ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = initF
        , view = rootV
        , subscriptions = subscriptions
        , update = update
        }
