module SecureVote.SPAs.SwarmMVP.Update exposing (..)

import Dict
import Material
import Maybe.Extra exposing ((?))
import SecureVote.Eth.Web3 exposing (..)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (ballotValToBytes, getSwmAddress)
import SecureVote.SPAs.SwarmMVP.Model exposing (LastPageDirection(PageBack, PageForward), Model, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (FromWeb3Msg(..), Msg(..), ToWeb3Msg(..))


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

        MultiMsg msgs ->
            multiUpdate msgs model []

        SetCandidateTx f ->
            { model | candidateTx = f model.candidateTx } ! []

        SetEthNode addr ->
            { model | ethNode = addr } ! []

        -- Errors
        LogErr err ->
            { model | errors = err :: model.errors } ! []

        -- PORTS
        ToWeb3 msg ->
            model ! updateToWeb3 msg model

        FromWeb3 msg ->
            updateFromWeb3 msg model

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model


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
