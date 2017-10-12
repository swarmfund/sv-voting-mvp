module SecureVote.SPAs.SwarmMVP.Views.SwmAddressV exposing (..)

import Dict
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options exposing (css, cs)
import Material.Textfield as Textf
import Material.Typography exposing (display1, display2, display3, headline, title)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(ChangePage))
import SecureVote.Components.UI.FullPageSlide exposing (fullPageSlide)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(Mdl, SetField))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(SwmVoteR))
import Maybe.Extra exposing ((?))


swmAddressV : Model -> Html Msg
swmAddressV model =
    let
        swmAddrId =
            "ethAddress"
    in
        fullPageSlide 384938493
            model
            []
            [ Card.text [ cs "center tc" ]
                [ Options.styled span [ display2, Color.text Color.black, cs "db pa2" ] [ text "Swarm Liquidity Vote" ]
                , Options.styled span [ headline, cs "black db pa2 mv3" ] [ text "Please enter your Swarm address below" ]
                , Textf.render Mdl
                    [ 83543983 ]
                    model.mdl
                    [ Options.onInput <| SetField swmAddrId
                    , Textf.label "Your Ethereum Address"
                    , Textf.floatingLabel
                    , Textf.value <| Dict.get swmAddrId model.fields ? ""
                    ]
                    []
                , div [ class "mt3" ]
                    [ btn 954898522 model [ SecBtn, Attr (class "ph2"), Click (ChangePage SwmVoteR) ] [ text "Skip" ]
                    , btn 894823489 model [ PriBtn, Attr (class "ph2"), Click (ChangePage SwmVoteR) ] [ text "Continue" ]
                    ]
                ]
            ]
