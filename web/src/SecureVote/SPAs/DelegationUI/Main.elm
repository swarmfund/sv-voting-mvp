module SecureVote.SPAs.DelegationUI.Main exposing (..)

import Html
import SecureVote.Eth.Web3 exposing (contractReadResponse, gotDelegatePayloadGen, metamaskTxidGen, onContractReadResponse, setWeb3Provider)
import SecureVote.SPAs.DelegationUI.Model exposing (Model, initModel)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(LogErr))
import SecureVote.SPAs.DelegationUI.MsgHandlers exposing (handleContractRead, handleDelegationPayload)
import SecureVote.SPAs.DelegationUI.Types exposing (Flags)
import SecureVote.SPAs.DelegationUI.Update exposing (update)
import SecureVote.SPAs.DelegationUI.Views.RootV exposing (rootV)


initF : Flags -> ( Model, Cmd Msg )
initF f =
    let
        m =
            initModel f
    in
    m ! [ setWeb3Provider m.eth.ethNode ]


subscriptions model =
    Sub.batch
        [ gotDelegatePayloadGen handleDelegationPayload
        , contractReadResponse (onContractReadResponse (handleContractRead model) LogErr)
        ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = initF
        , view = rootV
        , subscriptions = subscriptions
        , update = update
        }
