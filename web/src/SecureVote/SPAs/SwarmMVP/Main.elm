module SecureVote.SPAs.SwarmMVP.Main exposing (..)

import Html exposing (Html)
import SecureVote.Eth.Web3 exposing (gotWeb3Error, implErc20Balance, onIncomingErc20Balance, onIncomingWeb3Error, setWeb3Provider)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (setEthNodeTemp)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Update exposing (update)
import SecureVote.SPAs.SwarmMVP.Views.RootV exposing (rootView)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ implErc20Balance onIncomingErc20Balance
        , gotWeb3Error onIncomingWeb3Error
        ]


initCmds : Model -> List (Cmd Msg) -> Cmd Msg
initCmds initModel extraCmds =
    Cmd.batch <|
        [ setWeb3Provider initModel.ethNode
        ]
            ++ extraCmds


processedInitModelCmd : ( Model, Cmd Msg )
processedInitModelCmd =
    -- Note: Only use Msgs that do not
    -- send out commands
    update
        (MultiMsg
            [ setEthNodeTemp initModel.ethNode
            ]
        )
        initModel


main : Program Never Model Msg
main =
    let
        ( iModel, iCmd ) =
            processedInitModelCmd
    in
    Html.program
        { init = ( iModel, initCmds iModel [ iCmd ] )
        , view = rootView
        , subscriptions = subscriptions
        , update = update
        }
