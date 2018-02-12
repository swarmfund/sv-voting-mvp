module SecureVote.SPAs.SwarmMVP.Views.ListVotesV exposing (..)

import Dict
import Html exposing (Html, div, hr, span, text)
import Html.Attributes exposing (class)
import List exposing (filter, length, map, sortBy)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs, css)
import SecureVote.Components.UI.Elevation exposing (elevation)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Typo exposing (headline, subhead)
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
            List.sortBy .endTime <| filter (\{ startTime, endTime } -> startTime <= model.now && model.now < endTime) allBallots

        futureBallots =
            List.sortBy .startTime <| filter (\{ startTime, endTime } -> startTime > model.now && endTime > model.now) allBallots

        pastBallots =
            List.sortBy ((*) -1 << .endTime) <| filter (\{ startTime, endTime } -> model.now >= endTime) allBallots

        drawIfNotEmpty bs v =
            if length bs == 0 then
                []
            else
                v

        currBallotV =
            drawIfNotEmpty currentBallots <|
                [ subhead "Current Ballots"
                ]
                    ++ map drawBallotButton currentBallots

        futureBallotV =
            drawIfNotEmpty futureBallots <|
                [ subhead "Upcoming Ballots"
                ]
                    ++ map drawBallotButton futureBallots

        pastBallotV =
            drawIfNotEmpty pastBallots <|
                [ subhead "Past Ballots"
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
                ([ cs "ma3 ba b--light-silver"
                 , css "width" "auto"
                 , Options.onClick <| MultiMsg [ SetBallot ballot, PageGoForward OpeningSlideR ]
                 ]
                    ++ elevation id model
                )
                [ Card.title [ cs "pb0 mb0 w-100" ]
                    [ div [ class "dt w-100" ]
                        [ Options.styled span [ cs "b w-70 dtc tl" ] [ text ballotTitle ]
                        , Options.styled span [ cs "f6 dark-gray tr dtc" ] [ text voteTimeStatus ]
                        ]
                    ]
                , Card.text [ cs "tl dark-gray w-100" ]
                    [ text description
                    ]

                -- , Card.actions [ Card.border, cs "tl" ] [ styled span [ Typo.caption ] [ text voteStatus ] ]
                ]
    in
    fullPageSlide 3409830456
        model
        model.mainTitle
    <|
        currBallotV
            ++ futureBallotV
            ++ pastBallotV
