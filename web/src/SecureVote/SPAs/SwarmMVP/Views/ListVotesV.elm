module SecureVote.SPAs.SwarmMVP.Views.ListVotesV exposing (..)

import Dict
import Html exposing (Html, div, hr, span, text)
import Html.Attributes exposing (class, style)
import List exposing (filter, length, map, sortBy)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs, css)
import Maybe.Extra exposing ((?), isJust)
import SecureVote.Ballots.Types exposing (BallotSpec(..))
import SecureVote.Components.UI.Elevation exposing (elevation)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Loading exposing (loadingSpinner)
import SecureVote.Components.UI.Typo exposing (headline, subhead)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(OpeningSlideR))
import SecureVote.Utils.Time exposing (readableTime)


listVotesView : Model -> Html Msg
listVotesView model =
    let
        allBallots =
            sortBy
                (\b ->
                    case b of
                        BVerFF ->
                            99999999999999999

                        BVer01 bInner ->
                            bInner.startTime ? 0
                )
            <|
                Dict.values model.specToDeets

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

        totalBallots =
            Dict.get model.currDemoc model.democCounts

        foundNBallots =
            (Dict.get model.currDemoc model.democIssues |> Maybe.map Dict.size) ? 0

        gotNBallots =
            Dict.size model.specToDeets

        doneLoadingBallots =
            isJust totalBallots && totalBallots == Just gotNBallots

        allBallotsView =
            currBallotV ++ futureBallotV ++ pastBallotV

        noBallotsView =
            [ div [ class "v-mid center mb7 mt6" ] [ subhead "No ballots yet. Why don't you create one?" ] ]

        viewBallotsOrEmpty =
            if List.isEmpty allBallotsView then
                noBallotsView
            else
                allBallotsView
    in
    fullPageSlide 3409830456
        model
        model.mainTitle
    <|
        case ( totalBallots, foundNBallots, gotNBallots, doneLoadingBallots ) of
            ( Nothing, _, _, _ ) ->
                [ loadingBallots ]

            ( Just n, f, g, False ) ->
                [ ballotsProgress n f g ]

            _ ->
                viewBallotsOrEmpty


loadingBallots =
    div [ class "v-mid center" ]
        [ loadingSpinner "Loading details from blockchain..."
        ]


ballotsProgress n f g =
    div [ class "v-mid center" ]
        [ div [ class "f4" ] [ text <| "Loaded info for " ++ toString f ++ " of " ++ toString n ++ " ballots." ]
        , div [ class "f4" ] [ text <| "Loaded data for " ++ toString g ++ " of " ++ toString n ++ " ballots." ]
        , loadingSpinner ""
        ]
