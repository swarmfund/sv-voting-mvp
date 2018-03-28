module SecureVote.SPAs.SwarmMVP.Views.ListVotesV exposing (..)

import Bool.Extra as BE
import Dict
import Html exposing (Html, div, hr, span, text)
import Html.Attributes exposing (class, style)
import List exposing (filter, length, map, sortBy)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs, css)
import Maybe.Extra exposing ((?), isJust)
import Monocle.Common exposing ((=>), dict)
import SecureVote.Ballots.Types exposing (BallotSpec(..))
import SecureVote.Components.UI.Elevation exposing (elevation)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Loading exposing (loadingSpinner)
import SecureVote.Components.UI.Typo exposing (headline, subhead)
import SecureVote.Crypto.Hashing exposing (hashToInt)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (getUserErc20Addr)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToWeb3Msg(GetErc20Balance))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(OpeningSlideR))
import SecureVote.Utils.Lenses exposing (..)
import SecureVote.Utils.Time exposing (readableTime)
import Time
import Tuple exposing (first, second)


listVotesView : Model -> Html Msg
listVotesView model =
    let
        userAddr =
            getUserErc20Addr model

        prelimInfo =
            Dict.toList model.specToDeets
                |> List.map
                    (\( k, v ) ->
                        ( k
                        , (dictWDE model.currDemoc).getOption model.democIssues
                            |> Maybe.withDefault Dict.empty
                            |> Dict.toList
                            |> List.filter (\( i, bpi ) -> bpi.specHash == k)
                            |> List.map (\( i, bpi ) -> bpi.startTime)
                            |> List.head
                        )
                    )
                |> List.map (\( k, mst ) -> Maybe.map (\st -> ( k, st )) mst)
                |> List.filter Maybe.Extra.isJust
                |> Maybe.Extra.combine
                |> Maybe.withDefault []
                |> Dict.fromList

        allBallots =
            Dict.toList model.specToDeets
                |> List.map
                    (\( k, v ) ->
                        case v of
                            BVer01 i ->
                                Just ( k, i )

                            _ ->
                                Nothing
                    )
                |> filter isJust
                |> Maybe.Extra.combine
                |> Maybe.withDefault []

        haveVoted bHash =
            userAddr
                |> Maybe.andThen (\a -> (dict a => dict bHash).getOption model.haveVotedOn)
                |> (==) (Just True)

        currentBallots =
            List.sortBy (\( k, meh ) -> Dict.get k prelimInfo |> Maybe.withDefault 0) <|
                filter (second >> (\{ startTime, endTime } -> startTime <= model.now && model.now < endTime)) allBallots

        currVotedIn =
            List.filter (first >> haveVoted) currentBallots

        currNotVotedIn =
            List.filter (first >> haveVoted >> not) currentBallots

        futureBallots =
            List.sortBy (.startTime << second) <| filter (second >> (\{ startTime, endTime } -> startTime > model.now && endTime > model.now)) allBallots

        pastBallots =
            List.sortBy ((*) -1 << .endTime << second) <| filter (second >> (\{ startTime, endTime } -> model.now >= endTime)) allBallots

        drawIfNotEmpty bs v =
            if length bs == 0 then
                []
            else
                v

        currBallotNotVotedV =
            drawIfNotEmpty currNotVotedIn <|
                [ subhead "Ballots You Haven't Voted On"
                ]
                    ++ map (drawBallotButton { markVoted = Just False }) currNotVotedIn

        currBallotVotedV =
            drawIfNotEmpty currVotedIn <|
                [ subhead "Ballots You've Voted On"
                ]
                    ++ map (drawBallotButton { markVoted = Just True }) currVotedIn

        futureBallotV =
            drawIfNotEmpty futureBallots <|
                [ subhead "Upcoming Ballots"
                ]
                    ++ map (drawBallotButton { markVoted = Nothing }) futureBallots

        pastBallotV =
            drawIfNotEmpty pastBallots <|
                [ subhead "Past Ballots"
                ]
                    ++ map (drawBallotButton { markVoted = Nothing }) pastBallots

        drawBallotButton { markVoted } ( bHash, ballot ) =
            let
                { ballotTitle, startTime, endTime, shortDesc } =
                    ballot

                cardColor =
                    Color.color Color.Amber Color.S300

                markWith =
                    case markVoted of
                        Just True ->
                            text "✅ You have voted on this."

                        Just False ->
                            userAddr
                                |> Maybe.andThen (\addr -> (dict addr => dict bHash).getOption model.pendingVotes)
                                |> Maybe.map
                                    (\t ->
                                        if (toFloat model.now * Time.second) - t < 300 * Time.second then
                                            text "⏳ Waiting for vote to be confirmed..."
                                        else
                                            text "⚠️ Waiting for more than 5 minutes for vote to confirm. Check tx or vote again."
                                    )
                                |> Maybe.withDefault (text "🗳 You have not voted on this yet.")

                        Nothing ->
                            span [] []

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
                 , Options.onClick <| MultiMsg [ SetBallot bHash, ToWeb3 <| GetErc20Balance bHash, PageGoForward OpeningSlideR ]
                 ]
                    ++ elevation (hashToInt bHash) model
                )
                [ Card.title [ cs "pb0 mb0 w-100" ]
                    [ div [ class "dt w-100" ]
                        [ Options.styled span [ cs "b w-70 dtc tl" ] [ text ballotTitle ]
                        , Options.styled span [ cs "f6 dark-gray tr dtc" ] [ text voteTimeStatus, Html.br [] [], markWith ]
                        ]
                    ]
                , Card.text [ cs "tl dark-gray w-100" ]
                    [ text shortDesc
                    ]

                -- , Card.actions [ Card.border, cs "tl" ] [ styled span [ Typo.caption ] [ text voteStatus ] ]
                ]

        totalBallots =
            Dict.get model.currDemoc model.democCounts

        foundNBallots =
            (dict model.currDemoc).getOption model.democIToSpec |> Maybe.map Dict.size |> Maybe.withDefault 0

        gotNBallots =
            Dict.size model.specToDeets + gotNBadBallots

        gotNAbrvs =
            Dict.size model.erc20Abrvs + gotNBadBallots

        gotNBallotScDetails =
            Dict.size model.ballotScDetails

        gotNBadBallots =
            List.length model.fatalSpecFail

        gotNPrevVoteDeets =
            userAddr
                |> Maybe.andThen (\a -> (dict a).getOption model.haveVotedOn)
                |> Maybe.map Dict.size
                |> Maybe.withDefault 0

        doneLoadingBallots =
            let
                totBallots_ =
                    totalBallots ? 0
            in
            isJust totalBallots
                -- use <= not == here because we want to be okay with accidentally counting extra stuff (e.g. from errors with gotNBadBallots)
                && BE.all
                    (List.map ((<=) totBallots_)
                        [ gotNAbrvs
                        , gotNBallotScDetails
                        , foundNBallots
                        , gotNPrevVoteDeets
                        , gotNBallots
                        ]
                    )

        allBallotsView =
            currBallotNotVotedV ++ currBallotVotedV ++ futureBallotV ++ pastBallotV

        noBallotsView =
            [ div [ class "v-mid center mb7 mt6" ] [ subhead "No ballots yet. Why don't you create one?" ] ]

        viewBallotsOrEmpty =
            if List.isEmpty allBallotsView then
                noBallotsView
            else
                allBallotsView
    in
    fullPageSlide
        model
        { id = 3409830456
        , title = model.mainTitle
        , inner =
            case ( totalBallots, ( foundNBallots, gotNBallots, gotNAbrvs, gotNBallotScDetails, gotNPrevVoteDeets ), doneLoadingBallots ) of
                ( Nothing, _, _ ) ->
                    [ loadingBallots ]

                ( Just n, ns, False ) ->
                    [ ballotsProgress n ns ]

                _ ->
                    viewBallotsOrEmpty
        }


loadingBallots =
    div [ class "v-mid center" ]
        [ loadingSpinner "Loading details from blockchain..."
        ]


ballotsProgress n ( f, g, ga, gd, prevVs ) =
    let
        attrs =
            [ class "f4 mv2" ]
    in
    div [ class "v-mid center" ]
        [ subhead "Loading:"
        , div attrs [ text <| "Ballot Info " ++ toString f ++ " of " ++ toString n ++ " ballots." ]
        , div attrs [ text <| "Ballot Data " ++ toString g ++ " of " ++ toString n ++ " ballots." ]
        , div attrs [ text <| "ERC20 Details " ++ toString ga ++ " of " ++ toString n ++ " ballots." ]
        , div attrs [ text <| "Voting Details " ++ toString gd ++ " of " ++ toString n ++ " ballots." ]
        , div attrs [ text <| "Checking Past Votes " ++ toString prevVs ++ " of " ++ toString n ++ " ballots." ]
        , loadingSpinner ""
        ]
