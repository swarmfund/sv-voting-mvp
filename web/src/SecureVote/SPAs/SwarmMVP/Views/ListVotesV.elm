module SecureVote.SPAs.SwarmMVP.Views.ListVotesV exposing (..)

import Html exposing (Html, hr, span, text)
import List exposing (filter, length, map)
import Material.Card as Card
import Material.Options as Options exposing (cs, css)
import Material.Typography exposing (display2)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Typo exposing (headline)
import SecureVote.SPAs.SwarmMVP.Ballot exposing (allBallots)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg)


listVotesView : Model -> Html Msg
listVotesView model =
    let
        currentBallots =
            filter (\{ startTime, endTime } -> startTime <= model.now && model.now < endTime) allBallots

        futureBallots =
            filter (\{ startTime, endTime } -> startTime > model.now) allBallots

        pastBallots =
            filter (\{ startTime, endTime } -> model.now >= endTime) allBallots

        drawIfNotEmpty bs v =
            if length bs == 0 then
                []
            else
                v

        currBallotV =
            drawIfNotEmpty currentBallots <|
                [ headline "Current Ballots"
                ]
                    ++ map drawBallotButton currentBallots

        futureBallotV =
            drawIfNotEmpty futureBallots <|
                [ headline "Upcoming Ballots"
                ]
                    ++ map drawBallotButton futureBallots

        pastBallotV =
            drawIfNotEmpty pastBallots <|
                [ headline "Past Ballots"
                ]
                    ++ map drawBallotButton pastBallots

        drawBallotButton { ballotTitle, startTime, endTime } =
            text ballotTitle
    in
    fullPageSlide 3409830456
        model
        []
        [ Card.text [ cs "center tc" ] <|
            [ headline "SWM Governance"
            , Options.styled hr [] []
            ]
                ++ currBallotV
                ++ futureBallotV
                ++ pastBallotV
        ]
