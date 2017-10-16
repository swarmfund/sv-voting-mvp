module SecureVote.SPAs.SwarmMVP.Views.SwmCastVoteV exposing (..)

import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class)
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.List as Lists
import Material.Options as Options exposing (cs, css)
import Material.Slider as Slider
import Material.Typography exposing (display2, headline)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(ChangePage, SetDialog))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmSubmitR))


type alias BallotOption =
    { title : String
    , description : String
    , params : String
    , btnId : Int
    }


dialogView : String -> Html Msg
dialogView content =
    div [] [ text content ]


voteOptions : List BallotOption
voteOptions =
    [ BallotOption "Option 1" "Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long DescriptionReally long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long DescriptionReally long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description Really long Description " "" 865456735673
    , BallotOption "Option 2" "Description 2" "" 7456345623
    , BallotOption "Option 3" "Description 3" "" 7246474573
    , BallotOption "Option 4" "Description 4" "" 4573465672
    ]


castVoteView : Model -> Html Msg
castVoteView model =
    let
        optionList =
            List.map optionListItem voteOptions

        optionListItem { title, description, params, btnId } =
            Lists.li []
                [ Lists.content []
                    [ Slider.view
                        [ Slider.value 0
                        , Slider.min -3
                        , Slider.max 3

                        --, Slider.onChange SliderMsg
                        ]
                    , div [ class "dib" ] [ text title ]
                    ]
                , Lists.content2 []
                    [ btn btnId model [ SecBtn, Click (SetDialog (dialogView description)), OpenDialog ] [ text "Show Details" ]
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
