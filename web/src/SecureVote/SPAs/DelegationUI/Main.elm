module SecureVote.SPAs.DelegationUI.Main exposing (..)

import Html
import SecureVote.Eth.Subscriptions exposing (ethGenericSubs)
import SecureVote.Eth.Web3 exposing (contractReadResponse, gotDelegatePayloadGen, initWeb3WProvider, metamaskTxidGen, onContractReadResponse)
import SecureVote.SPAs.DelegationUI.Model exposing (Model, initModel)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(..))
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
    m ! [ initWeb3WProvider m.eth.ethNode ]


subscriptions model =
    Sub.batch
        [ gotDelegatePayloadGen handleDelegationPayload
        , contractReadResponse (onContractReadResponse (handleContractRead model) LogErr)
        , ethGenericSubs Web3
        ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = initF
        , view = rootV
        , subscriptions = subscriptions
        , update = update
        }
