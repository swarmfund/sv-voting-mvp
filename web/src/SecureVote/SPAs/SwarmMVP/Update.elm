module SecureVote.SPAs.SwarmMVP.Update exposing (..)

import Dict exposing (fromList)
import Dom.Scroll exposing (toTop)
import List.Extra exposing (zip)
import Material
import Material.Helpers as MHelp exposing (map1st, map2nd)
import Material.Snackbar as Snackbar
import Maybe exposing (andThen)
import Maybe.Extra exposing ((?))
import RemoteData exposing (RemoteData(Failure, Loading, NotAsked, Success))
import SecureVote.Crypto.Curve25519 exposing (encryptBytes)
import SecureVote.Eth.Utils exposing (keccak256OverString)
import SecureVote.Eth.Web3 exposing (..)
import SecureVote.SPAs.SwarmMVP.Ballot exposing (doBallotOptsMatch)
import SecureVote.SPAs.SwarmMVP.Ballots.ProdBallots exposing (getChainIndexFor)
import SecureVote.SPAs.SwarmMVP.Ballots.ReleaseSchedule exposing (doBallotOptsMatchRSched, voteOptionsRSched)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (ballotValToBytes, defaultDelegate, getDelegateAddress, getUserErc20Addr)
import SecureVote.SPAs.SwarmMVP.Model exposing (LastPageDirection(PageBack, PageForward), Model, resetAllBallotFields)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromCurve25519Msg(..), FromWeb3Msg(..), Msg(..), ToCurve25519Msg(..), ToWeb3Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (defaultRoute)
import SecureVote.SPAs.SwarmMVP.Types exposing (TxidCheckStatus(TxidFail, TxidInProgress, TxidSuccess))
import SecureVote.SPAs.SwarmMVP.VotingCrypto.RangeVoting exposing (constructBallot, orderedBallotBits)
import Task exposing (attempt)


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
            let
                val =
                    (f <| Dict.get id model.ballotRange) ? 0

                newBallotRange =
                    Dict.insert id val model.ballotRange

                newBallotBits =
                    Dict.insert id (ballotValToBytes val) model.ballotBits
            in
            { model | ballotRange = newBallotRange, ballotBits = newBallotBits } ! []

        SetBallot b ->
            let
                oTitles =
                    List.map .title b.voteOptions

                ( m_, cmds_ ) =
                    let
                        auditMsgM =
                            if b.endTime < model.now then
                                [ DoAudit b ]
                            else
                                []

                        midMsgs =
                            MultiMsg <| [ ToWeb3 <| ReInit oTitles ] ++ auditMsgM
                    in
                    update midMsgs <| resetAllBallotFields { model | currentBallot = b } b
            in
            m_ ! [ cmds_ ]

        DoAudit b ->
            model ! [ auditCmd model b ]

        ConstructBallotPlaintext ->
            let
                plainBytesM =
                    orderedBallotBits model model.ballotBits
                        |> Maybe.andThen
                            (flip constructBallot <|
                                getDelegateAddress model
                                    ? defaultDelegate
                            )

                skM =
                    Maybe.map .hexSk model.keypair

                remotePkM =
                    model.remoteHexPk

                encCmds =
                    case ( skM, remotePkM, plainBytesM ) of
                        ( Just sk, Just pk, Just bs ) ->
                            [ encryptBytes { hexSk = sk, hexRemotePk = pk, bytesToSign = bs } ]

                        _ ->
                            []
            in
            { model | ballotPlaintext = plainBytesM } ! encCmds

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

        VoteWMetaMask ->
            model ! [ castMetaMaskVote model.candidateTx ]

        -- ToCurve25519 msg ->
        --     updateToCurve25519 msg model
        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model


auditCmd model b =
    getBallotResults { ethUrl = model.ethNode, ethRPCAuth = "", votingAddr = b.contractAddr, erc20Addr = b.erc20Addr }


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
            model ! [ setWeb3Provider model.ethNode, getEncryptionPublicKey model.currentBallot.contractAddr ]

        GetErc20Balance ->
            let
                addr =
                    -- probs okay because it will return 0
                    getUserErc20Addr model ? "0x00"
            in
            model ! [ getErc20Balance <| GetErc20BalanceReq model.currentBallot.erc20Addr addr (getChainIndexFor model.currentBallot.id) ]

        CheckTxid txid ->
            { model | txidCheck = TxidInProgress } ! [ checkTxid txid ]

        ReInit oTitles ->
            model ! [ getInit { addr = model.currentBallot.contractAddr, oTitles = oTitles }, getEncryptionPublicKey model.currentBallot.contractAddr ]


updateFromWeb3 : FromWeb3Msg -> Model -> ( Model, Cmd Msg )
updateFromWeb3 msg model =
    case msg of
        GotBalance bal ->
            let
                cBallot =
                    model.currentBallot
            in
            { model | currentBallot = { cBallot | erc20Balance = Just bal } } ! []

        GotDataParam data ->
            let
                candTx =
                    model.candidateTx
            in
            { model | ballotAllDone = True, candidateTx = { candTx | data = Just data } } ! []

        GotEncPubkey encPk ->
            { model | remoteHexPk = Just encPk } ! []

        Web3Init init ->
            { model | miniVotingAbi = init.miniAbi } ! []

        GetBallotOptsLegacy resp ->
            case resp of
                Success opts ->
                    let
                        optCheck =
                            model.currentBallot.voteOptions == voteOptionsRSched && doBallotOptsMatchRSched opts
                    in
                    ballotOptSuccess model { isGood = optCheck, hashes = [ "no need for hashes in legacy" ] }

                _ ->
                    ballotOptElse model resp

        GetBallotOpts resp ->
            case resp of
                Success d ->
                    ballotOptSuccess model d

                _ ->
                    ballotOptElse model resp

        GetBallotPeriod resp ->
            let
                cb_ =
                    model.currentBallot

                { cb, cmds } =
                    case resp of
                        Success { startTime, endTime } ->
                            -- if the ballot has actually ended, but the UI had the wrong time, AND that wrong time was in the future, ONLY THEN trigger an audit
                            if endTime < model.now && model.now <= cb_.endTime then
                                { cb = { cb_ | startTime = startTime, endTime = endTime }
                                , cmds = [ auditCmd model cb_ ]
                                }
                            else
                                { cb = cb_, cmds = [] }

                        _ ->
                            { cb = cb_, cmds = [] }
            in
            { model | ballotOpen = resp, currentBallot = cb } ! cmds

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
                    model_ ! [ constructDataParam { encBallot = encBytes, voterPubkey = keypair.hexPk, votingContractAddr = voteCAddr } ]

                _ ->
                    update (LogErr "Unable to create Ethereum tx data parameter - missing encrypted ballot, keypair, or voting contract address.") model_



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


ballotOptSuccess : Model -> { isGood : Bool, hashes : List String } -> ( Model, Cmd Msg )
ballotOptSuccess model { isGood, hashes } =
    if isGood then
        let
            b =
                model.currentBallot

            optHashToTitle =
                fromList <| zip hashes <| List.map .title b.voteOptions
        in
        { model | ballotVerificationPassed = Success isGood, optHashToTitle = optHashToTitle } ! []
    else
        mFail model "Release schedule options in voting interface do not match smart contract!" ! []


ballotOptElse : Model -> RemoteData String a -> ( Model, Cmd Msg )
ballotOptElse model resp =
    case resp of
        Success _ ->
            mFail model "Response from Web3 successful but decoding the response failed. This error should never happen." ! []

        Failure errMsg ->
            mFail model errMsg ! []

        Loading ->
            { model | ballotVerificationPassed = Loading } ! []

        NotAsked ->
            { model | ballotVerificationPassed = NotAsked } ! []
