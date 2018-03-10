module SecureVote.SPAs.SwarmMVP.Views.HowToVoteV exposing (..)

import Html exposing (Html, div, li, p, span, text, ul)
import Html.Attributes exposing (class, style)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs)
import Material.Typography as Typo exposing (display2)
import Maybe.Extra exposing ((?))
import SecureVote.Ballots.Lenses exposing (bShortDesc)
import SecureVote.Ballots.Types exposing (BallotSpec)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.Components.UI.Typo exposing (headline, subhead)
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotParams)
import SecureVote.SPAs.SwarmMVP.Model exposing (..)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(PageGoForward))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(..))


combinedHowToVoteCopy : String -> Model -> List (Html Msg)
combinedHowToVoteCopy bHash model =
    let
        rangeVotingCopy =
            [ "Each vote consists of a choosing a score within the range -3 to +3."
            , "+3 indicates best option and -3 indicates the worst option."
            , "When the voting has finished, all votes are weighted and summed, and the option with the highest weighted score wins."
            ]

        submitVoteCopy =
            [ "Once you have finished selecting values for your vote options, your ballot will be encrypted. "
            , "You will then be presented with instructions to submit your vote either via MyEtherWallet, MetaMask, or another wallet, and to validate the integrity of your ballot if you wish."
            ]

        ballotExplanationSection =
            case ( model.route, (mBSpec bHash).getOption model ) of
                ( ListAllVotesR, _ ) ->
                    div [] []

                ( _, Just b ) ->
                    div []
                        [ subhead "What Is This Ballot?"
                        , p [] [ text <| bShortDesc.getOption b ? "Error: Ballot Not Found" ]
                        ]

                _ ->
                    div [] []

        rangeVotingSection =
            div []
                [ subhead "How to Use Range Voting"
                , ul [] <| List.map (\copy -> li [] [ text copy ]) rangeVotingCopy
                ]

        submitVoteSection =
            div []
                [ subhead "How Can I Submit My Vote?"
                , text <| String.concat submitVoteCopy
                ]
    in
    [ ballotExplanationSection
    , rangeVotingSection
    , submitVoteSection
    ]


howToVoteView : Model -> ( String, BallotSpec ) -> Html Msg
howToVoteView model ( bHash, currBallot ) =
    fullPageSlide
        model
        { id = 3453456456
        , title = "How To Vote"
        , inner =
            [ div [ class "mw7 center tl" ] <| combinedHowToVoteCopy bHash model
            , btn 5475855442 model [ PriBtn, Attr (class "mv3"), Click (PageGoForward SwmVoteR) ] [ text "Continue" ]
            ]
        }
