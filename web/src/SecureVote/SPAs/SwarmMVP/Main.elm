module SecureVote.SPAs.SwarmMVP.Main exposing (..)

import Html exposing (Html)
import SecureVote.Ballots.SpecSource exposing (gotFailedSpecFromIpfsHandler, gotSpecFromIpfsHandler)
import SecureVote.Crypto.Curve25519 exposing (..)
import SecureVote.Eth.Msg exposing (EthMsg(RefreshMMAddress, SetMMAddress))
import SecureVote.Eth.Web3 exposing (..)
import SecureVote.LocalStorage exposing (..)
import SecureVote.SPAs.SwarmMVP.Fields exposing (..)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (..)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromCurve25519Msg(..), FromWeb3Msg(..), Msg(..))
import SecureVote.SPAs.SwarmMVP.SubHandlers exposing (..)
import SecureVote.SPAs.SwarmMVP.Types exposing (Flags)
import SecureVote.SPAs.SwarmMVP.Update exposing (update)
import SecureVote.SPAs.SwarmMVP.Views.RootV exposing (rootView)
import SecureVote.SPAs.SwarmMVP.Web3Handler exposing (..)
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
        , gotTxidCheckStatus onGotTxidStatus
        , gotAuditMsg
        , gotMetamask
        , metamaskTxid
        , democNVotesSub
        , gotBallotInfoSub
        , gotSpecFromIpfsHandler GotFullSpecFromIpfs
        , gotFailedSpecFromIpfsHandler GotFailSpecFromIpfs
        , gotErc20AbrvHandler
        , ballotInfoExtraHandler
        , localStorageSub lsGetHandler
        , localStorageErrSub lsFailHandler
        , contractReadResponse (onContractReadResponse (cReadHandler model) LogErr)
        , every (15 * second) (\_ -> CheckForPrevVotes)
        , every (5 * second) (flip (/) 1000 >> round >> SetTime)
        , every (1 * second) (\_ -> Web3 RefreshMMAddress)
        , gotMMAddress (Web3 << SetMMAddress)
        ]


initCmds : Model -> Flags -> List (Cmd Msg) -> Cmd Msg
initCmds initModel { democHash, indexABI, indexAddr, ballotBoxABI } extraCmds =
    Cmd.batch <|
        [ setWeb3Provider initModel.ethNode
        , perform (SetTime << round << (\t -> t / 1000)) Time.now
        , genKeyPair True
        , getDemocHashes
            { democHash = democHash
            , indexABI = indexABI
            , indexAddr = indexAddr
            , ballotBoxABI = ballotBoxABI
            }
        , getMMAddress ()
        , getLsOnLoad
        ]
            ++ extraCmds


getLsOnLoad =
    Cmd.batch <|
        List.map getLocalStorage
            [ lsAddrId
            , lsBallotsVotedId
            , lsPendingVotesId
            ]


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
    ( iModel, initCmds iModel flags [ iCmd ] )


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = initF
        , view = rootView
        , subscriptions = subscriptions
        , update = update
        }
