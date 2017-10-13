module SecureVote.SPAs.SwarmMVP.Views.SwmCastVoteV exposing (..)

import Html exposing (Html, div, span, text, p)
import Html.Attributes exposing (class)
import Material.Card as Card
import Material.Color as Color
import Material.List as Lists
import Material.Options as Options exposing (cs)
import Material.Typography exposing (display2, headline)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(ChangePage))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmSubmitR))


type alias BallotOption =   
    { title: String
    , description: String
    , params: String
    }

voteOptions : List BallotOption
voteOptions =
    [ BallotOption "Option 1" "Description 1" ""
    , BallotOption "Option 2" "Description 2" ""
    , BallotOption "Option 3" "Description 3" ""
    , BallotOption "Option 4" "Description 4" ""
    ]


castVoteView : Model -> Html Msg
castVoteView model =
    let
        optionList =
            List.map optionListItem voteOptions

        optionListItem {title, description, params} =
            Lists.li []
                [ Lists.content [] [ text "checkbox" ]
                , Lists.content2 []
                    [ text title
                    , p [] [ text description ]
                    ]
                ]
    in 
        fullPageSlide 657980946
            model
            []
            [ Card.text [ cs "center tc" ]
                [ Options.styled span [ display2, Color.text Color.black, cs "db pa2" ] [ text "Swarm Liquidity Vote" ]
                , Options.styled span [ headline, cs "black dib ba pa3 ma3" ] [ text "Swarm Token Balance: 34,228" ]
                , Lists.ul [ cs "mw7 center" ] optionList
                , btn 894823489 model [ PriBtn, Attr (class "mv3"), Click (ChangePage SwmSubmitR) ] [ text "Continue" ]
                ]
            ]