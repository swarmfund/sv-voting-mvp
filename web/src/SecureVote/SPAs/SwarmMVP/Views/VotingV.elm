module SecureVote.SPAs.SwarmMVP.Views.VotingV exposing (..)

import Dict
import Html exposing (Html, b, br, code, div, li, p, pre, span, table, td, text, tr, ul)
import Html.Attributes exposing (class, id, style)
import Json.Encode exposing (encode)
import Material.Card as Card
import Material.Options as Options exposing (cs)
import Material.Typography as Typo exposing (display2, title)
import Maybe.Extra exposing ((?))
import RemoteData exposing (RemoteData(Success))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Typo exposing (headline, subhead)
import SecureVote.Eth.Encoders exposing (minEthTxEncoder)
import SecureVote.Eth.Models exposing (CandidateEthTx)
import SecureVote.Eth.Utils exposing (processCandidateTx)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (formatTsAsDate, getDelegateAddress)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(GethDialog, MEWDialog, VerifyDialog), Route(SwmSubmitR))


votingView : Model -> Html Msg
votingView model =
    let
        tableRow ( desc, value ) =
            tr []
                [ td [ class "b tr" ] [ text <| desc ++ ": " ]
                , td [ class "tl", style [ ( "word-wrap", "break-word" ) ] ] [ text value ]
                ]

        getResults { id, title, description } =
            ( title
            , toString <| Dict.get id model.ballotRange ? 0
            )

        displayResults =
            table [ class "mt2 dt--fixed w-auto-l" ]
                (List.map tableRow
                    (List.map getResults model.currentBallot.voteOptions)
                    ++ [ tableRow ( "Delegate", getDelegateAddress model ? "None" ) ]
                )

        ( startTime, endTime ) =
            case model.ballotOpen of
                Success { startTime, endTime } ->
                    ( startTime, endTime + (15 * 60) )

                -- should be this value anyway, included as safe default
                _ ->
                    ( 1509372000, 1510009200 + (15 * 60) )

        ballotClosedWarning =
            if startTime > model.now then
                Options.styled div [ Typo.headline, cs "red mv3" ] [ text <| "Warning! Ballot not yet open! Opens on " ++ formatTsAsDate startTime ]
            else
                div [] []

        voteBtnsOrTxid model =
            case model.metamaskTxid of
                Nothing ->
                    div [ class "mv2" ]
                        [ btn 758678435 model [ SecBtn, Attr (class "ph3"), Click (SetDialog "Cast your vote using MyEtherWallet" MEWDialog), OpenDialog ] [ text "Cast using M.E.W." ]
                        , btn 483974399 model [ SecBtn, Attr (class "ph3"), Click VoteWMetaMask ] [ text "Cast using MetaMask" ]
                        , btn 785784536 model [ SecBtn, Attr (class "ph3"), Click (SetDialog "Cast using Other" GethDialog), OpenDialog ] [ text "Cast using Other" ]
                        ]

                Just txid ->
                    div [ class "mv2 f5" ] [ text "Vote cast using MetaMask.", br [] [], text <| "TXID: " ++ txid ]

        endBtns model =
            div []
                [ btn 987572349 model [ PriBtn, Attr (class "mv3 mh2"), Click (SetDialog "Verify Your Ballot" VerifyDialog), OpenDialog ] [ text "Verify Ballot" ]
                , btn 843973394 model [ SecBtn, Attr (class "mv3 mh2"), Click PageGoHome ] [ text "Back to Start" ]
                ]

        ballotDetails =
            div []
                [ subhead "Ballot Transaction:"
                , div [ class "mw7 center" ] [ pre [ class "tl" ] [ text <| candTxText model.candidateTx ] ]
                , ballotClosedWarning
                , span [ class "" ] [ subhead <| "Results available " ++ formatTsAsDate endTime ]
                , voteBtnsOrTxid model
                , endBtns model
                ]

        loadingSpinner =
            div [ id "loading-screen" ]
                [ text "Waiting for ballot encryption..."
                , div [ class "cssload-container cssload-orange cssload-small" ]
                    [ ul [ class "cssload-flex-container" ]
                        [ li []
                            [ span [ class "cssload-loading cssload-one" ] []
                            , span [ class "cssload-loading cssload-two" ] []
                            , span [ class "cssload-loading-center" ] []
                            ]
                        ]
                    ]
                ]

        ballotDetailsSection =
            if model.ballotAllDone then
                ballotDetails
            else
                loadingSpinner
    in
    fullPageSlide 923844759
        model
        []
        [ Card.text [ cs "center tc" ]
            [ div [ class "pv2" ] [ headline "Cast Your Vote" ]
            , div [ class "ba dib pa2 ma1", style [ ( "min-width", "50%" ) ] ]
                [ subhead "Ballot Summary:"
                , displayResults
                ]
            , ballotDetailsSection
            ]
        ]


candTxText : CandidateEthTx -> String
candTxText candTx =
    let
        minTx =
            processCandidateTx candTx
    in
    case minTx of
        Nothing ->
            "Error generating Tx details"

        Just rec ->
            encode 2 <| minEthTxEncoder rec



--generateMewUri : Model -> String
--generateMewUri model =
--    let
--        candTx =
--            model.candidateTx
--
--        baseUrl =
--            "https://www.myetherwallet.com/?"
--
--        from =
--            "from=" ++ candTx.from ? ""
--
--        to =
--            "&to=" ++ candTx.to ? ""
--
--        value =
--            "&value=" ++ toString candTx.value
--
--        gas =
--            "&gas=" ++ candTx.gas
--
--        data =
--            "&data=" ++ candTx.data ? ""
--
--        tail =
--            "#send-transaction"
--    in
--    baseUrl ++ from ++ to ++ value ++ data ++ tail
