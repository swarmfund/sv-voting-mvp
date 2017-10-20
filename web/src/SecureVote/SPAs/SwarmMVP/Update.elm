module SecureVote.SPAs.SwarmMVP.Update exposing (..)

import Dict
import Material
import Material.Helpers as MHelp exposing (map1st, map2nd)
import Material.Snackbar as Snackbar
import Maybe.Extra exposing ((?))
import SecureVote.Crypto.Curve25519 exposing (encryptBytes)
import SecureVote.Eth.Web3 exposing (..)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (ballotValToBytes, getSwmAddress)
import SecureVote.SPAs.SwarmMVP.Model exposing (LastPageDirection(PageBack, PageForward), Model, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromCurve25519Msg(..), FromWeb3Msg(..), Msg(..), ToWeb3Msg(..))
import SecureVote.SPAs.SwarmMVP.VotingCrypto.RangeVoting exposing (constructBallot, orderedBallotBits)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetElevation id isOn ->
            { model | elevations = Dict.insert id isOn model.elevations } ! []

        SetField fieldName value ->
            { model | fields = Dict.insert fieldName value model.fields } ! []

        PageGoForward route ->
            { model | route = route, history = model.route :: model.history, lastPageDirection = PageForward } ! []

        PageGoBack ->
            { model
                | route = List.head model.history ? initModel.route
                , history = List.tail model.history ? []
                , lastPageDirection = PageBack
                , lastRoute = Just model.route
            }
                ! []

        SetDialog title route ->
            { model | dialogHtml = { title = title, route = route } } ! []

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

        ConstructBallotPlaintext ->
            let
                plainBytesM =
                    orderedBallotBits model.ballotBits |> Maybe.andThen (flip constructBallot "0xa74476443119A942dE498590Fe1f2454d7D4aC0d")

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
            model ! updateToWeb3 msg model

        FromWeb3 msg ->
            updateFromWeb3 msg model

        FromCurve25519 msg ->
            updateFromCurve25519 msg model

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model


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


updateToWeb3 : ToWeb3Msg -> Model -> List (Cmd msg)
updateToWeb3 web3msg model =
    case web3msg of
        SetProvider ->
            [ setWeb3Provider model.ethNode ]

        GetErc20Balance ->
            let
                swmAddr =
                    -- probs okay because it will return 0
                    getSwmAddress model ? "0x00"
            in
            [ getErc20Balance <| GetErc20BalanceReq model.swarmErc20Address swmAddr ]


updateFromWeb3 : FromWeb3Msg -> Model -> ( Model, Cmd Msg )
updateFromWeb3 msg model =
    case msg of
        GotBalance bal ->
            { model | swmBalance = Just bal } ! []


updateFromCurve25519 : FromCurve25519Msg -> Model -> ( Model, Cmd Msg )
updateFromCurve25519 msg model =
    case msg of
        GotKey kp ->
            { model | keypair = Just kp } ! []

        GotEncBytes bs ->
            { model | encBytes = Just bs } ! []
