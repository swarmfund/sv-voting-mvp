module SecureVote.SPAs.SwarmMVP.Views.SwmHowToVoteV exposing (..)

import Html exposing (Html, div, li, p, span, text, ul)
import Html.Attributes exposing (class, style)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs)
import Material.Typography exposing (display2, headline)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(PageGoForward))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmVoteR))


combinedHowToVoteCopy : List (Html Msg)
combinedHowToVoteCopy =
    let
        ballotExplanationCopy =
            "This ballot is to decide on the release schedule of the SWM token."

        rangeVotingCopy =
            [ "Each vote consists of a choosing a score within the range -3 to +3."
            , "+3 indicates best option and -3 indicates the worst option."
            , "When the voting has finished, all votes are weighted and summed, and the option with the highest weighted score wins."
            ]

        submitVoteCopy =
            [ "Once you have finished selecting values for your vote options, your ballot will be encrypted. "
            , "You will then be presented with instructions to submit your vote either via MyEtherWallet or another wallet, and to validate the integrity of your ballot if you wish."
            ]

        ballotExplanationSection =
            div []
                [ Options.styled span [ headline, cs "black db mv3" ] [ text "What is this Ballot?" ]
                , text ballotExplanationCopy
                ]

        rangeVotingSection =
            div []
                [ Options.styled span [ headline, cs "black db mv3" ] [ text "How to use Range Voting" ]
                , ul [] <| List.map (\copy -> li [] [ text copy ]) rangeVotingCopy
                ]

        submitVoteSection =
            div []
                [ Options.styled span [ headline, cs "black db mv3" ] [ text "How can I submit my vote?" ]
                , text <| String.concat submitVoteCopy
                ]
    in
    [ ballotExplanationSection
    , rangeVotingSection
    , submitVoteSection
    ]


howToVoteView : Model -> Html Msg
howToVoteView model =
    fullPageSlide 3453456456
        model
        []
        [ Card.text [ cs "center tc" ]
            [ Options.styled span [ display2, Color.text Color.black, cs "db pa2 heading-text" ] [ text "How To Vote" ]
            , div [ class "mw7 center tl" ]
                combinedHowToVoteCopy
            , btn 5475855442 model [ PriBtn, Attr (class "mv3"), Click (PageGoForward SwmVoteR) ] [ text "Continue" ]
            ]
        ]
