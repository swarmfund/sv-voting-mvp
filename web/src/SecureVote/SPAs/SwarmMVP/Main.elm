module SecureVote.SPAs.SwarmMVP.Main exposing (..)

import Html exposing (Html)
import SecureVote.Crypto.Curve25519 exposing (genKeyPair, onIncomingCurve25519Error, onIncomingEncBytes, onIncomingKeyPair, receiveCurve25519Error, receiveEncryptedBytes, receiveKeyPair)
import SecureVote.Eth.Web3 exposing (contractReadResponse, getBallotResults, getEncryptionPublicKey, getInit, gotAuditMsg, gotEncPubkey, gotMetamask, gotTxidCheckStatus, gotWeb3Error, implDataParam, implErc20Balance, implInit, metamaskTxid, onContractReadResponse, onGotPubkey, onGotTxidStatus, onIncomingErc20Balance, onIncomingWeb3Error, onInit, onRecieveDataParam, performContractRead, setWeb3Provider)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (setEthNodeTemp)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model, devEthNode, initEthNode, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromCurve25519Msg(..), FromWeb3Msg(..), Msg(..))
import SecureVote.SPAs.SwarmMVP.Types exposing (Flags)
import SecureVote.SPAs.SwarmMVP.Update exposing (update)
import SecureVote.SPAs.SwarmMVP.Views.RootV exposing (rootView)
import SecureVote.SPAs.SwarmMVP.Web3Handler exposing (decodeRead, readOptsErr)
import Task exposing (perform)
import Time exposing (every, second)


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
        , contractReadResponse <| onContractReadResponse decodeRead readOptsErr
        , gotTxidCheckStatus onGotTxidStatus
        , gotAuditMsg
        , gotMetamask
        , metamaskTxid
        ]


initCmds : Model -> List (Cmd Msg) -> Cmd Msg
initCmds initModel extraCmds =
    let
        votingAddr =
            initModel.currentBallot.contractAddr
    in
    Cmd.batch <|
        [ setWeb3Provider initModel.ethNode
        , perform (SetTime << round << (\t -> t / 1000)) Time.now
        , genKeyPair True
        , getDemocHashes
        ]
            ++ extraCmds


processedInitModelCmd : Flags -> ( Model, Cmd Msg )
processedInitModelCmd fs =
    -- Note: Only use Msgs that do not
    -- send out commands
    let
        model =
            initModel fs
    in
    update
        (MultiMsg
            [ setEthNodeTemp model.ethNode
            ]
        )
        model


initF : Flags -> ( Model, Cmd Msg )
initF flags =
    let
        ( iModel, iCmd ) =
            processedInitModelCmd flags
    in
    ( iModel, initCmds iModel [ iCmd ] )


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = initF
        , view = rootView
        , subscriptions = subscriptions
        , update = update
        }
