module SecureVote.SPAs.SwarmMVP.Views.VotingV exposing (..)

import Dict
import Html exposing (Html, b, code, div, li, p, pre, span, table, td, text, tr, ul)
import Html.Attributes exposing (class, id, style)
import Json.Encode exposing (encode)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs)
import Material.Typography exposing (display2, headline, title)
import Maybe.Extra exposing ((?))
import RemoteData exposing (RemoteData(Success))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Eth.Encoders exposing (minEthTxEncoder)
import SecureVote.Eth.Models exposing (CandidateEthTx)
import SecureVote.Eth.Utils exposing (processCandidateTx)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (formatTsAsDate, getDelegateAddress)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(PageGoForward, SetDialog))
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
                Options.styled div [ headline, cs "red mv3" ] [ text <| "Warning! Ballot not yet open! Opens on " ++ formatTsAsDate startTime ]
            else
                div [] []

        ballotDetails =
            div []
                [ Options.styled div [ headline, cs "black" ] [ text "Ballot Transaction:" ]
                , div [ class "mw7 ph3 center" ] [ pre [ class "tl" ] [ text <| candTxText model.candidateTx ] ]
                , ballotClosedWarning
                , Options.styled div [ title, cs "black mv3" ] [ text <| "Results available " ++ formatTsAsDate endTime ]
                , div [ class "mv4" ]
                    [ btn 758678435 model [ SecBtn, Attr (class "ph3"), Click (SetDialog "Cast your vote using MyEtherWallet" MEWDialog), OpenDialog ] [ text "Cast using M.E.W." ]
                    , btn 785784536 model [ SecBtn, Attr (class "ph3"), Click (SetDialog "Cast using Other" GethDialog), OpenDialog ] [ text "Cast using Other" ]
                    ]
                , btn 987572349 model [ PriBtn, Attr (class "mv3"), Click (SetDialog "Verify Your Ballot" VerifyDialog), OpenDialog ] [ text "Verify Ballot" ]
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
            [ Options.styled span [ display2, Color.text Color.black, cs "db pa2 heading-text" ] [ text "Cast Your Vote" ]
            , div [ class "ba dib pa3 ma3", style [ ( "min-width", "50%" ) ] ]
                [ Options.styled span
                    [ headline, cs "black" ]
                    [ text "Ballot Summary:" ]
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
