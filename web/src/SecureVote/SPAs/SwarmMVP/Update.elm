module SecureVote.SPAs.SwarmMVP.Update exposing (..)

import Dict exposing (fromList)
import Dom.Scroll exposing (toTop)
import Either exposing (Either(Right))
import Json.Decode as D
import Json.Encode as E
import List.Extra exposing (zip)
import Material
import Material.Helpers as MHelp exposing (map1st, map2nd)
import Material.Snackbar as Snackbar
import Maybe exposing (andThen)
import Maybe.Extra exposing ((?), isJust, isNothing)
import Monocle.Common exposing ((<|>), (=>), dict, maybe)
import RemoteData exposing (RemoteData(Failure, Loading, NotAsked, Success))
import SecureVote.Ballots.Lenses exposing (..)
import SecureVote.Ballots.SpecSource exposing (CidType(Sha256), getBallotSpec)
import SecureVote.Ballots.Types exposing (BallotSpec)
import SecureVote.Crypto.Curve25519 exposing (encryptBytes)
import SecureVote.Eth.Update exposing (ethUpdate)
import SecureVote.Eth.Utils exposing (isValidEthAddress, keccak256OverString, toHex)
import SecureVote.Eth.Web3 exposing (..)
import SecureVote.LocalStorage exposing (LsMsg(LsGeneral), getLocalStorage, lsUpdate, setLocalStorage)
import SecureVote.SPAs.SwarmMVP.Ballot exposing (doBallotOptsMatch)
import SecureVote.SPAs.SwarmMVP.Ballots.ReleaseSchedule exposing (doBallotOptsMatchRSched, voteOptionsRSched)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Fields exposing (..)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (ballotValToBytes, getDelegateAddress, getUserErc20Addr, resetAllBallotFields)
import SecureVote.SPAs.SwarmMVP.Model exposing (..)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromCurve25519Msg(..), FromWeb3Msg(..), Msg(..), ToCurve25519Msg(..), ToWeb3Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (defaultRoute)
import SecureVote.SPAs.SwarmMVP.Types exposing (TxidCheckStatus(TxidFail, TxidInProgress, TxidSuccess))
import SecureVote.SPAs.SwarmMVP.VotingCrypto.RangeVoting exposing (constructBallot, orderedBallotBits)
import SecureVote.Utils.DecodeP exposing (dDictDict)
import SecureVote.Utils.Encode exposing (encDictDict)
import SecureVote.Utils.Int exposing (maxInt)
import SecureVote.Utils.Lenses exposing ((=|>), dictWDE)
import SecureVote.Utils.Ports exposing (mkCarry)
import SecureVote.Utils.Update exposing (doUpdate)
import Task exposing (attempt)
import Time
import Tuple exposing (second)


scrollToTop : String -> Cmd Msg
scrollToTop id =
    attempt (\_ -> NoOp) (toTop id)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetTime time ->
            { model | now = time } ! []

        SetElevation id isOn ->
            { model | elevations = Dict.insert id isOn model.elevations } ! []

        SetField fieldName value ->
            { model | fields = Dict.insert fieldName value model.fields } ! []

        RemoveFieldVal k ->
            { model | fields = Dict.remove k model.fields } ! []

        SetBoolField k v ->
            { model | boolFields = Dict.insert k v model.boolFields } ! []

        PageGoForward route ->
            { model | route = route, history = model.route :: model.history, lastPageDirection = PageForward, lastRoute = Nothing } ! [ scrollToTop "sv-main" ]

        PageGoBack ->
            { model
                | route = List.head model.history ? defaultRoute
                , history = List.tail model.history ? []
                , lastPageDirection = PageBack
                , lastRoute = Just model.route
            }
                ! [ scrollToTop "sv-main" ]

        PageGoHome ->
            { model
                | route = defaultRoute
                , history = []
                , lastPageDirection = PageBack
                , lastRoute = Just model.route
            }
                ! [ scrollToTop "sv-main" ]

        SetDialog title route ->
            { model | dialogHtml = { title = title, route = route } } ! [ scrollToTop "dialog-container" ]

        SetBallotRange id value ->
            let
                val =
                    round value
            in
            { model
                | ballotRange = Dict.insert id val model.ballotRange
                , ballotBits = Dict.insert id (ballotValToBytes val) model.ballotBits
            }
                ! []

        ModBallotRange id f ->
            update (SetBallotRange id <| toFloat <| f <| Dict.get id model.ballotRange ? 0) model

        SetBallot bHash ->
            let
                bSpecM =
                    Dict.get bHash model.specToDeets

                contractAddr =
                    (dictWDE model.currDemoc => dict bHash =|> bpiVotingAddr).getOption model.democIssues ? "Unknown Contract Addr"

                ( m_, cmds_ ) =
                    update NoOp <| resetAllBallotFields { model | currentBallot = Just bHash } { contractAddr = contractAddr }

                doAuditIfBallotEnded =
                    case bSpecM of
                        Just bSpec ->
                            if (bEndTime.getOption bSpec ? maxInt) < model.now then
                                [ auditCmd { model | currentBallot = Just bHash } ( bHash, bSpec ) ]
                            else
                                []

                        _ ->
                            []
            in
            m_ ! ([ cmds_ ] ++ doAuditIfBallotEnded)

        ConstructBallotPlaintext ->
            let
                plainBytesM =
                    mBHashBSpecPair model
                        |> Result.fromMaybe "Cannot get Ballot Spec from model - have you selected a ballot?"
                        |> Result.andThen
                            (\( bHash, bSpec ) ->
                                orderedBallotBits ( bHash, bSpec ) model.ballotBits
                                    |> Result.andThen
                                        (flip constructBallot defaultDelegate)
                            )

                plainHexBytes =
                    plainBytesM
                        |> Result.toMaybe
                        |> Maybe.andThen toHex

                skM =
                    Maybe.map .hexSk model.keypair

                remotePkM =
                    mBHashBSpecPair model |> Maybe.andThen (second >> bEncPK.getOption)

                voteAddr =
                    mBHashBSpecPair model |> Maybe.andThen (\( bHash, _ ) -> (mVotingAddr bHash).getOption model)

                ( msg, encCmds ) =
                    case ( skM, remotePkM, plainBytesM, plainHexBytes, voteAddr ) of
                        ( Just sk, Just pk, Ok bs, _, _ ) ->
                            ( NoOp, [ encryptBytes { hexSk = sk, hexRemotePk = pk, bytesToSign = bs } ] )

                        ( _, Nothing, _, Just bs, Just voteAddr_ ) ->
                            ( NoOp, [ constructDataParam { ballot = bs, useEnc = False, voterPubkey = "", votingContractAddr = voteAddr_, abi = model.ballotBoxABI } ] )

                        ( _, _, Err s, _, _ ) ->
                            ( LogErr <| "Something went wrong generating BallotPlaintext: " ++ s, [] )

                        ( _, _, _, _, _ ) ->
                            ( LogErr <| "Fatal error generating BallotPlaintext!", [] )

                ( m_, cmd_ ) =
                    update msg { model | ballotPlaintext = plainBytesM, ballotRawHex = plainHexBytes }
            in
            m_ ! (encCmds ++ [ cmd_ ])

        GotFullSpecFromIpfs res ->
            case res of
                Ok { bHash, bSpec, cid } ->
                    let
                        startTimeFromSC =
                            (dictWDE model.currDemoc => dict bHash =|> bpiStartTime).getOption model.democIssues

                        updatedBSpec =
                            case startTimeFromSC of
                                Just sTs ->
                                    bStartTime.set sTs bSpec

                                Nothing ->
                                    bSpec

                        newSpecDeets =
                            Dict.insert bHash updatedBSpec model.specToDeets

                        erc20AbrvCmd =
                            case bErc20Addr.getOption bSpec of
                                Just e20Addr ->
                                    if isNothing (Dict.get bHash model.erc20Abrvs) then
                                        [ getErc20Abrv { bHash = bHash, erc20Addr = e20Addr } ]
                                    else
                                        []

                                Nothing ->
                                    []
                    in
                    { model | specToDeets = newSpecDeets } ! (erc20AbrvCmd ++ [])

                Err e ->
                    fatalFailedSpecUpdate e model

        GotFailSpecFromIpfs res ->
            case res of
                Ok { bHash, err } ->
                    { model | failedSpec = Dict.insert bHash err model.failedSpec } ! []

                Err e ->
                    fatalFailedSpecUpdate e model

        MarkBallotVoted b { voterM, bHash } ->
            Maybe.Extra.or voterM (getUserErc20Addr model)
                |> Maybe.map
                    (\addr ->
                        let
                            m_ =
                                { model | haveVotedOn = (dictWDE addr => dict bHash).set b model.haveVotedOn }
                        in
                        m_ ! [ setLocalStorage { key = lsBallotsVotedId, value = E.encode 0 <| encDictDict E.bool m_.haveVotedOn } ]
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        CheckForPrevVotes ->
            let
                cmds =
                    getUserErc20Addr model
                        |> Maybe.Extra.filter isValidEthAddress
                        |> Maybe.map
                            (\vAddr ->
                                Dict.toList ((dict model.currDemoc).getOption model.democIssues ? Dict.empty)
                                    |> List.map
                                        (\( bHash, bDetails ) ->
                                            if (dictWDE vAddr => dict bHash).getOption model.haveVotedOn /= Just True then
                                                performContractRead
                                                    { addr = bpiVotingAddr.get bDetails
                                                    , abi = model.ballotBoxABI
                                                    , carry = mkCarry <| E.string bHash
                                                    , method = "voterToBallotID"
                                                    , args = [ E.string vAddr ]
                                                    }
                                            else
                                                Cmd.none
                                        )
                            )
                        |> Maybe.withDefault []
            in
            model ! cmds

        MarkBallotTxInProg ->
            ( model
            , Task.perform
                (\t ->
                    model.currentBallot
                        |> Maybe.andThen (\bHash -> Maybe.map (\addr -> { addr = addr, bHash = bHash }) (getUserErc20Addr model))
                        |> Maybe.map (\addrBHash -> SetBallotProgTime addrBHash t)
                        |> Maybe.withDefault NoOp
                )
                Time.now
            )

        SetBallotProgTime { addr, bHash } t ->
            let
                m_ =
                    { model | pendingVotes = (dictWDE addr => dict bHash).set t model.pendingVotes }
            in
            m_ ! [ setLocalStorage { key = lsPendingVotesId, value = E.encode 0 <| encDictDict E.float m_.pendingVotes } ]

        MultiMsg msgs ->
            multiUpdate msgs model []

        SetCandidateTx f ->
            { model | candidateTx = f model.candidateTx } ! []

        SetEthNode addr ->
            { model | ethNode = addr } ! []

        -- Errors
        LogErr err ->
            addSnack err model

        Snackbar msg_ ->
            Snackbar.update msg_ model.snack
                |> map1st (\s -> { model | snack = s })
                |> map2nd (Cmd.map Snackbar)

        -- PORTS
        ToWeb3 msg ->
            updateToWeb3 msg model

        FromWeb3 msg ->
            updateFromWeb3 msg model

        FromCurve25519 msg ->
            updateFromCurve25519 msg model

        FromAuditor msg ->
            { model | auditMsgs = msg :: model.auditMsgs } ! []

        LS msg ->
            lsWatchers msg <| doUpdate LS lsUpdate msg model.lsBucket (\b -> { model | lsBucket = b })

        Web3 msg ->
            ethUpdate Web3 msg model

        VoteWMetaMask ->
            model ! [ castMetaMaskTx model.candidateTx ]

        -- ToCurve25519 msg ->
        --     updateToCurve25519 msg model
        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model


auditCmd model ( bHash, bSpec ) =
    let
        votingAddr =
            mCurrVotingAddr.get model

        erc20Addr =
            bErc20Addr.getOption bSpec ? "NO ERC20 ADDR"
    in
    getBallotResults { ethUrl = model.ethNode, ethRPCAuth = "", votingAddr = votingAddr, erc20Addr = erc20Addr }


fatalFailedSpecUpdate : String -> Model -> ( Model, Cmd Msg )
fatalFailedSpecUpdate e model =
    update (LogErr e) { model | fatalSpecFail = e :: model.fatalSpecFail }


addSnack : String -> Model -> ( Model, Cmd Msg )
addSnack err model =
    let
        ( snackbar_, effect ) =
            Snackbar.add (Snackbar.toast "some payload" err) model.snack
                |> map2nd (Cmd.map Snackbar)

        model_ =
            { model | snack = snackbar_, errors = err :: model.errors }
    in
    ( model_, Cmd.batch [ effect ] )


multiUpdate : List Msg -> Model -> List (Cmd Msg) -> ( Model, Cmd Msg )
multiUpdate msgs model cmds =
    case msgs of
        msg :: msgs_ ->
            let
                ( model_, cmd ) =
                    update msg model

                cmds_ =
                    cmds ++ [ cmd ]
            in
            multiUpdate msgs_ model_ cmds_

        [] ->
            ( model, Cmd.batch cmds )


updateToWeb3 : ToWeb3Msg -> Model -> ( Model, Cmd Msg )
updateToWeb3 web3msg model =
    case web3msg of
        SetProvider ->
            model
                ! [ setWeb3Provider model.ethNode ]

        GetErc20Balance bHash ->
            let
                addr =
                    -- probs okay because it will return 0
                    getUserErc20Addr model ? "0x00"

                blockN =
                    Maybe.map (toString << .startingBlockEst) (Dict.get bHash model.ballotScDetails) ? "latest"
            in
            model ! defaultOrB model [] (\b -> Maybe.map (\erc20Addr -> [ getErc20Balance <| GetErc20BalanceReq erc20Addr addr blockN model.delegationABI model.delegationAddr ]) (bErc20Addr.getOption b) ? [])

        CheckTxid txid ->
            { model | txidCheck = TxidInProgress } ! [ checkTxid txid ]


doUpdateErr : String -> Model -> ( Model, Cmd Msg )
doUpdateErr msg m =
    update (LogErr msg) m


updateFromWeb3 : FromWeb3Msg -> Model -> ( Model, Cmd Msg )
updateFromWeb3 msg model =
    case msg of
        GotBalance bal ->
            { model | erc20Balance = Just bal } ! []

        GotDataParam data ->
            let
                candTx =
                    model.candidateTx
            in
            { model | ballotAllDone = True, candidateTx = { candTx | data = Just data } } ! []

        GotEncPubkey encPk ->
            { model | remoteHexPk = Just encPk } ! []

        GotTxidStatus txidE ->
            case txidE of
                Ok { data, confirmed, gas, logMsg } ->
                    if confirmed then
                        -- gas should be around 114,000 for a successful vote, though can be lower for a repeat vote.
                        if gas < 80000 then
                            let
                                errMsg =
                                    "Warning: Gas low! "
                                        ++ (if String.length logMsg > 0 then
                                                "Voting contract returned error: " ++ logMsg
                                            else
                                                "Gas seems low, vote likely failed to be recorded. Check ethereum transaction log & manual verification; unable to find an error message."
                                           )
                            in
                            { model | txidCheck = TxidFail errMsg } ! []
                        else if Just data == model.candidateTx.data then
                            { model | txidCheck = TxidSuccess } ! []
                        else
                            { model | txidCheck = TxidFail <| "Warning! Transaction data does not match expected! Please try again using txData: " ++ (model.candidateTx.data ? "<Error: tx data not found in voting interface! You should probably never see this message.") } ! []
                    else
                        { model | txidCheck = TxidFail "Transaction not yet confirmed" } ! []

                Err msg ->
                    { model | txidCheck = TxidFail msg } ! []

        GotMetaMask ->
            { model | metamask = True } ! []

        GotMetaMaskTxid txid ->
            { model | metamaskTxid = Just txid } ! []

        GotBallotCount r ->
            case r of
                Success { democHash, n } ->
                    { model | democCounts = Dict.insert democHash n model.democCounts } ! []

                _ ->
                    doUpdateErr "Unable to read democracy from the blockchain! Please try reloading the page." model

        GotBallotInfo r ->
            case r of
                Success { democHash, i, bHash, extraData, votingContract, startTime } ->
                    let
                        ballotPrelim =
                            BallotPrelimInfo bHash votingContract extraData startTime

                        di =
                            (dictWDE democHash => dict bHash).set ballotPrelim model.democIssues

                        dIToSpec =
                            (dictWDE democHash => dict i).set bHash model.democIToSpec

                        voterAddr =
                            Debug.log "userErc20Addr" <| getUserErc20Addr model

                        checkVotesCmd =
                            case voterAddr of
                                Nothing ->
                                    Cmd.none

                                Just vAddr ->
                                    performContractRead
                                        { addr = votingContract
                                        , abi = model.ballotBoxABI
                                        , carry = mkCarry <| E.string bHash
                                        , method = "voterToBallotID"
                                        , args = [ E.string vAddr ]
                                        }
                    in
                    { model | democIssues = di, democIToSpec = dIToSpec } ! [ getBallotSpec { bHash = bHash, cidType = Sha256 }, checkVotesCmd ]

                _ ->
                    doUpdateErr "Got bad ballot info from blockchain. Please check your connection or reload the page" model

        GotErc20Abrv { erc20Addr, bHash, abrv } ->
            { model | erc20Abrvs = Dict.insert bHash abrv model.erc20Abrvs } ! []

        GotBallotSCDeets scDeets ->
            { model | ballotScDetails = Dict.insert scDeets.bHash scDeets model.ballotScDetails } ! []


updateFromCurve25519 : FromCurve25519Msg -> Model -> ( Model, Cmd Msg )
updateFromCurve25519 msg model =
    case msg of
        GotKey kp ->
            { model | keypair = Just kp } ! []

        GotEncBytes bs ->
            let
                model_ =
                    { model | encBytes = Just bs }
            in
            case ( model_.encBytes, model_.keypair, model_.candidateTx.to ) of
                ( Just encBytes, Just keypair, Just voteCAddr ) ->
                    model_ ! [ constructDataParam { ballot = encBytes, useEnc = True, voterPubkey = keypair.hexPk, votingContractAddr = voteCAddr, abi = model.ballotBoxABI } ]

                _ ->
                    doUpdateErr "Unable to create Ethereum tx data parameter - missing encrypted ballot, keypair, or voting contract address." model_



-- updateToCurve25519 : ToCurve25519Msg -> Model -> ( Model, Cmd Msg )
-- updateToCurve25519 msg model =
--     case msg of
--         GenerateSignedBallot ->
--             case ( model.ballotPlaintext, model.keypair, model.remoteHexPk ) of
--                 ( Just bytes, Just keypair, Just hexRemotePk ) ->
--                     model ! [ encryptBytes { bytesToSign = bytes, hexSk = keypair.hexSk, hexRemotePk = hexRemotePk } ]
--                 _ ->
--                     update (LogErr "App does not have all of: ballot plaintext, keypair, and encryption public key. Unable to generate encrypted ballot.") model


mFail : Model -> String -> Model
mFail model errMsg =
    { model
        | ballotVerificationPassed = Failure errMsg
    }


defaultOrB : Model -> a -> (BallotSpec -> a) -> a
defaultOrB model default f =
    model.currentBallot
        |> Maybe.andThen (\h -> (mBSpec h).getOption model)
        |> Maybe.map f
        |> Maybe.withDefault default


lsWatchers : LsMsg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
lsWatchers msg ( m, c ) =
    (case msg of
        LsGeneral ( k, v ) ->
            List.map
                (\( lsId, mF ) ->
                    if k == lsId then
                        Just (mF v)
                    else
                        Nothing
                )
                [ ( lsBallotsVotedId
                  , \v_ ->
                        D.decodeString (dDictDict D.bool) v_
                            |> Result.map (\d -> ( { m | haveVotedOn = d }, c ))
                  )
                , ( lsPendingVotesId
                  , \v_ ->
                        D.decodeString (dDictDict D.float) v_
                            |> Result.map (\d -> ( { m | pendingVotes = d }, c ))
                  )
                ]
                |> List.filter isJust
                |> List.head
                |> Maybe.Extra.join
                |> (\rM ->
                        case rM of
                            Nothing ->
                                Ok ( m, c )

                            Just r ->
                                r
                   )

        _ ->
            Ok ( m, c )
    )
        |> Result.mapError (Debug.log "LocalStorage watchers error: ")
        |> Result.withDefault ( m, c )
