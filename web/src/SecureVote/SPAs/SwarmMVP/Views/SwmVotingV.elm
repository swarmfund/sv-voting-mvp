module SecureVote.SPAs.SwarmMVP.Views.SwmVotingV exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (cs)
import Material.Typography exposing (display2, headline)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(ChangePage, SetDialog))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmSubmitR))


dialogView1 : Html Msg
dialogView1 =
    div [] [ text "Hello World 1" ]


dialogView2 : Html Msg
dialogView2 =
    div [] [ text "Hello World 2" ]


votingView : Model -> Html Msg
votingView model =
    fullPageSlide 923844759
        model
        []
        [ Card.text [ cs "center tc" ]
            [ Options.styled span [ display2, Color.text Color.black, cs "db pa2" ] [ text "Swarm Liquidity Vote" ]
            , div [ class "ba dib pa3 ma3", style [ ( "min-width", "50%" ) ] ]
                [ Options.styled span
                    [ headline, cs "black" ]
                    [ text "Ballot Details:" ]
                , div [] [ text "You selected: Option 1" ]
                ]
            , Options.styled span [ headline, cs "black db" ] [ text "Ballot Transaction:" ]
            , div [] [ text "{\"value\": 0x0, \"data\": 0x2b59cOf..." ]
            , div [] [ text "\"from\": 0xbc64..., \"gas\": 0x289442}" ]
            , div [ class "mv4" ]
                [ btn 758678435 model [ SecBtn, Attr (class "ph3"), Click (ChangePage SwmSubmitR) ] [ text "Cast using MEW" ]
                , btn 785784536 model [ SecBtn, Attr (class "ph3"), OpenDialog, Click (SetDialog dialogView1) ] [ text "Cast using GETH" ]
                ]
            , btn 987572349 model [ PriBtn, Attr (class "mv3"), OpenDialog, Click (SetDialog dialogView2) ] [ text "Verify Ballot" ]
            ]
        ]
