module SecureVote.SPAs.SwarmMVP.Main exposing (..)

import Html exposing (Html)
import SecureVote.Crypto.Curve25519 exposing (genKeyPair, onIncomingCurve25519Error, onIncomingEncBytes, onIncomingKeyPair, receiveCurve25519Error, receiveEncryptedBytes, receiveKeyPair)
import SecureVote.Eth.Web3 exposing (getEncryptionPublicKey, getInit, gotEncPubkey, gotWeb3Error, implDataParam, implErc20Balance, implInit, onGotPubkey, onIncomingErc20Balance, onIncomingWeb3Error, onInit, onRecieveDataParam, setWeb3Provider)
import SecureVote.SPAs.SwarmMVP.Const exposing (votingContractAddr)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (setEthNodeTemp)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromCurve25519Msg(..), FromWeb3Msg(..), Msg(..))
import SecureVote.SPAs.SwarmMVP.Update exposing (update)
import SecureVote.SPAs.SwarmMVP.Views.RootV exposing (rootView)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ implErc20Balance onIncomingErc20Balance
        , gotWeb3Error onIncomingWeb3Error
        , receiveKeyPair <| onIncomingKeyPair (FromCurve25519 << GotKey) LogErr
        , receiveEncryptedBytes <| onIncomingEncBytes (FromCurve25519 << GotEncBytes) LogErr
        , receiveCurve25519Error <| onIncomingCurve25519Error LogErr
        , implDataParam <| onRecieveDataParam
        , gotEncPubkey <| onGotPubkey
        , implInit <| onInit (FromWeb3 << Web3Init)
        ]


initCmds : Model -> List (Cmd Msg) -> Cmd Msg
initCmds initModel extraCmds =
    Cmd.batch <|
        [ setWeb3Provider initModel.ethNode
        , genKeyPair True
        , getEncryptionPublicKey votingContractAddr
        , getInit True
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
