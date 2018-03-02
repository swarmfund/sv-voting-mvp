module SecureVote.SPAs.AdminUI.Update exposing (..)

import Dict
import Element.Input exposing (dropMenu, menu, select, selected, updateSelection)
import Maybe.Extra exposing ((?))
import SecureVote.Ballots.Types exposing (emptyBSpec01)
import SecureVote.SPAs.AdminUI.Components.Input exposing (genDropSelect)
import SecureVote.SPAs.AdminUI.Model exposing (Model, Web3Model, initWeb3Model)
import SecureVote.SPAs.AdminUI.Msg exposing (FromWeb3Msg(..), Msg(..), ToWeb3Msg(..))
import SecureVote.SPAs.AdminUI.Views.BallotBuilder exposing (bSpecToJson, bSpecValueToString, buildBSpecV01)
import Task
import Web3.Types exposing (Error(Error), Sha3(Sha3))
import Web3.Utils exposing (sha3)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            model ! []

        SetStrField k v ->
            let
                sf =
                    if v == "" then
                        Dict.remove k model.strFields
                    else
                        Dict.insert k v model.strFields
            in
            { model | strFields = sf } ! []

        SetBoolField k v ->
            { model | boolFields = Dict.insert k v model.boolFields } ! []

        SelectBallot selectMsg ->
            let
                s =
                    model.select ? genDropSelect

                m_ =
                    { model | select = Just (updateSelection selectMsg s), workingBallot = emptyBSpec01, web3 = initWeb3Model }
            in
            update UpdateWrkBallot m_

        SelectOptType sMsg ->
            let
                s =
                    model.selectOpts ? dropMenu Nothing SelectOptType

                m_ =
                    { model | selectOpts = Just <| updateSelection sMsg s }
            in
            update UpdateWrkBallot m_

        UpdateWrkBallot ->
            let
                newWorkingBallot =
                    buildBSpecV01 model

                jsonBallotStr =
                    bSpecValueToString <| bSpecToJson newWorkingBallot
            in
            { model | workingBallot = newWorkingBallot, jsonBallot = jsonBallotStr } ! [ Task.attempt handleSha3Response (sha3 jsonBallotStr) ]

        UpdateSha3 s ->
            { model | sha3 = s } ! []

        Sha3Error s ->
            { model | sha3 = "Warning! Error from Keccak256 hash of Ballot: " ++ s } ! []

        SaveJson ->
            model ! []

        LogErr err ->
            { model | errors = err :: model.errors } ! []

        MMsg msgs ->
            case msgs of
                msg_ :: msgRem ->
                    let
                        ( m_, cmds_ ) =
                            update msg_ model

                        ( m, cmds ) =
                            update (MMsg msgRem) m_
                    in
                    ( m, Cmd.batch [ cmds_, cmds ] )

                [] ->
                    model ! []

        FromWeb3 w3Msg ->
            let
                ( web3, cmds ) =
                    w3Update w3Msg model.web3
            in
            ( { model | web3 = web3 }, cmds )

        ToWeb3 w3Msg ->
            let
                ( web3, cmds ) =
                    toW3Update w3Msg model.web3
            in
            ( { model | web3 = web3 }, cmds )


handleSha3Response : Result Error Sha3 -> Msg
handleSha3Response r =
    case r of
        Ok (Sha3 s) ->
            UpdateSha3 s

        Err (Error s) ->
            Sha3Error s


w3Update : FromWeb3Msg -> Web3Model -> ( Web3Model, Cmd Msg )
w3Update msg model =
    case msg of
        GotTxid txid ->
            { model | txid = Just txid } ! []


toW3Update : ToWeb3Msg -> Web3Model -> ( Web3Model, Cmd Msg )
toW3Update msg model =
    case msg of
        SendTxToMM tx ->
            model ! []
