module SecureVote.SPAs.SwarmMVP.Views.ListVotesV exposing (..)

import Dict
import Html exposing (Html, hr, span, text)
import List exposing (filter, length, map, sortBy)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs, css)
import SecureVote.Components.UI.Elevation exposing (elevation)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Typo exposing (headline)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(OpeningSlideR))
import SecureVote.Utils.Time exposing (readableTime)


listVotesView : Model -> Html Msg
listVotesView model =
    let
        allBallots =
            sortBy .startTime <| Dict.values model.allBallots

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

        drawBallotButton ballot =
            let
                { id, ballotTitle, startTime, endTime, description } =
                    ballot

                cardColor =
                    Color.color Color.Amber Color.S300

                voteTimeStatus =
                    case ( compare model.now startTime, compare model.now endTime ) of
                        ( LT, _ ) ->
                            "Ballot opens in " ++ readableTime startTime model

                        ( _, GT ) ->
                            "Ballot closed " ++ readableTime endTime model ++ " ago"

                        _ ->
                            "Ballot closes in " ++ readableTime endTime model
            in
            Card.view
                ([ cs "ma4 ba b--light-silver"
                 , css "width" "auto"
                 , Options.onClick <| MultiMsg [ SetBallot ballot, PageGoForward OpeningSlideR ]
                 ]
                    ++ elevation id model
                )
                [ Card.title [ cs "b pb1" ] [ text ballotTitle ]
                , Card.text [ cs "tl dark-gray w-100" ]
                    [ text description
                    , Options.styled hr [ cs "mv1 mh0" ] []
                    , Options.styled span
                        [ cs "tr fr dark-gray f6"
                        ]
                        [ text voteTimeStatus ]
                    ]

                -- , Card.actions [ Card.border, cs "tl" ] [ styled span [ Typo.caption ] [ text voteStatus ] ]
                ]
    in
    fullPageSlide 3409830456
        model
        []
        [ Card.text [ cs "center tc" ] <|
            [ headline model.mainTitle
            , Options.styled hr [] []
            ]
                ++ currBallotV
                ++ futureBallotV
                ++ pastBallotV
        ]
