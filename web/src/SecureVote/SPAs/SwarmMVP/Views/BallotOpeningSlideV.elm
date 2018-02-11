module SecureVote.SPAs.SwarmMVP.Views.BallotOpeningSlideV exposing (..)

import Html exposing (Html, a, div, em, p, span, strong, text)
import Html.Attributes exposing (class, href, style, target)
import Material.Card as Card
import Material.Color as Color exposing (Hue(Red), Shade(S500))
import Material.Options as Options exposing (cs, css)
import Material.Typography exposing (body1, display1, display2, display3, headline, title)
import RemoteData exposing (RemoteData(Failure, Loading, NotAsked, Success))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Eth.Types exposing (AuditDoc(..))
import SecureVote.SPAs.SwarmMVP.Helpers exposing (formatTsAsDate)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToWeb3Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(..))


openingSlide : Model -> Html Msg
openingSlide model =
    let
        introText =
            [ [ text "This ballot is to decide the release schedule for SWM tokens." ]
            , [ text "You will be presented with a number of options in the form "
              , em [] [ text "X releases of Y days." ]
              , text " If X were 8, this would mean that tokens will be released in 8 distinct events, with each releasing 12.5% of all tokens. If Y were 42, that means that each release event will occur every 42 days. Each option has a 'details' button that will give you specifc information about the proposed release schedule."
              ]
            , [ text "When you vote, you allocate each option a number from -3 to +3 (inclusive). It's important to choose a vote for each option. (This method of voting is called 'Range Voting'.)" ]
            , [ text "If you'd like, ", a [ href "https://www.youtube.com/watch?v=afEwklJEzFc", target "_blank" ] [ text "here is a video" ], text " walking you through the voting process." ]
            , [ text "When you're ready, let's begin the voting process!" ]
            ]

        resultsText =
            [ [ text "Results are in!" ]
            , [ strong [] [ text "Winner: " ], text "4 releases of 84 days each" ]
            , []
            , [ strong [] [ text "Raw Results:" ] ]
            , [ text "8 releases of 42 days each:     with # votes:       215861632749216557139154" ]
            , [ text "42 releases of 8 days each:     with # votes:     -6028150571595824835708731" ]
            , [ text "16 releases of 42 days each:    with # votes:    -10257849820412608278569577" ]
            , [ text "4 releases of 84 days each:     with # votes:      7845588400895824835708731" ]
            ]

        introParagraphs =
            div [ class "center" ] <| List.map renderPara introText

        resultsParas =
            div [ class "center" ] <| List.map renderPara resultsText

        renderPara txt =
            Options.styled span [ body1, cs "black db pa1 mv2" ] txt

        renderAuditMsg auditMsg =
            let
                wrapper attrs msg =
                    Options.styled span ([ cs "db" ] ++ attrs) [ text msg ]
            in
            case auditMsg of
                AuditLog msg ->
                    wrapper [] msg

                AuditLogErr msg ->
                    wrapper [ cs "red" ] msg

                AuditFail msg ->
                    wrapper [ cs "red bold" ] msg

                AuditSuccess res ->
                    wrapper [] <| toString res

        renderAuditLog model =
            List.map renderAuditMsg model.auditMsgs
    in
    fullPageSlide 9483579329
        model
        []
        [ Card.text [ cs "center tc" ]
            [ Options.styled div [ display2, Color.text Color.black, cs "pa2 heading-text" ] [ text "Welcome to the SWM Release Schedule Vote" ]
            , Options.styled div [ headline, cs "black pa2 mv3" ] [ text "This vote is open to all Swarm token holders." ]
            , div [] (renderAuditLog model)
            , div
                [ style [ ( "max-width", "700px" ) ], class "center" ]
                [ resultsParas
                , ballotIntegrity model
                , div [ class "mv3" ]
                    [ btn 348739845 model [ PriBtn, Attr (class "ph2"), Click (PageGoForward SwmAddressR) ] [ text "Continue" ]
                    ]
                ]
            ]
        ]


ballotIntegrity : Model -> Html Msg
ballotIntegrity model =
    let
        failMsg errMsg =
            Options.styled span [ body1, cs "red" ] [ text errMsg ]

        successMsg msg =
            Options.styled span [ body1, cs "green" ] [ text msg ]

        ballotOptionsHtml =
            List.singleton <|
                case model.ballotVerificationPassed of
                    NotAsked ->
                        text "Initializing... (We should never see this in the UI)"

                    Loading ->
                        text "Loading..."

                    Success b ->
                        if b then
                            successMsg "✅ Ballot options match."
                        else
                            failMsg <| "Verification failed: " ++ "❌ Warning! Ballot options do not match!"

                    Failure s ->
                        failMsg <| "Verification failed: " ++ s

        row left right =
            div [ class "w-100 dt dt--fixed black mv2" ]
                [ div [ class "dtc tr pr3" ] left
                , div [ class "dtc tl" ] right
                ]

        ballotOpenHtml =
            List.singleton <|
                case model.ballotOpen of
                    NotAsked ->
                        text "Initializing... (We should never see this in the UI)"

                    Loading ->
                        text "Loading..."

                    Failure e ->
                        failMsg <| "Error: " ++ e

                    Success { startTime, endTime } ->
                        if startTime > model.now then
                            failMsg <| "🔜 Ballot has not opened for voting yet. (Opens " ++ formatTsAsDate startTime ++ " local time)"
                        else if endTime < model.now then
                            failMsg "❌ Voting is closed."
                        else
                            successMsg <| "✅ Voting open! 🗳 Voting closes on: " ++ formatTsAsDate endTime ++ " local time"
    in
    div [ class "mt1 mb3" ]
        [ Options.styled div [ cs "black mb2", headline ] [ text "Checking ballot details:" ]
        , row
            [ text "Ballot open:" ]
            ballotOpenHtml
        , row
            [ text "Ballot options match smart contract:" ]
            ballotOptionsHtml
        ]
