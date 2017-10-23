module SecureVote.SPAs.SwarmMVP.Views.SwmHowToVoteV exposing (..)

import Html exposing (Html, div, p, span, text)
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


loremIpsum : String
loremIpsum =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur sagittis semper mi, sit amet laoreet mauris lobortis sed. Sed facilisis justo non sagittis sodales. Proin ornare interdum euismod. Cras ultricies ante vitae convallis viverra. Donec dapibus odio ac metus consequat consectetur vel quis massa. Nunc mattis feugiat erat at porta. Praesent varius felis non ullamcorper condimentum. Ut vitae posuere massa. Aenean vitae euismod mauris. Nunc turpis augue, porttitor at massa eget, gravida vehicula nisl.\n\n     Aliquam erat volutpat. Nunc viverra arcu velit, nec tincidunt purus pulvinar vitae. In nec felis scelerisque, condimentum purus laoreet, ornare mi. Sed rutrum feugiat fermentum. Nullam ut augue in urna fringilla maximus vel vitae mi. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Sed tincidunt consectetur quam, a placerat dui volutpat in. Fusce maximus tortor nec urna faucibus fringilla quis quis nibh. Sed felis ligula, lacinia sit amet ipsum in, tincidunt finibus orci. Integer nibh dui, porta non elit vitae, ornare maximus arcu. Vestibulum rutrum vulputate eleifend."


howToVoteView : Model -> Html Msg
howToVoteView model =
    fullPageSlide 3453456456
        model
        []
        [ Card.text [ cs "center tc" ]
            [ Options.styled span [ display2, Color.text Color.black, cs "db pa2 heading-text" ] [ text "Swarm Liquidity Vote" ]
            , Options.styled span [ headline, cs "black db pa2 mv3" ] [ text "How To Vote" ]
            , div [ class "mw7 ph3 overflow-visible center" ] [ p [ class "tl" ] [ text loremIpsum ] ]
            , btn 5475855442 model [ PriBtn, Attr (class "mv3"), Click (PageGoForward SwmVoteR) ] [ text "Continue" ]
            ]
        ]
