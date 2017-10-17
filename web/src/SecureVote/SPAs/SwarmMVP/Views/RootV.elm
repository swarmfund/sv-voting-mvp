module SecureVote.SPAs.SwarmMVP.Views.RootV exposing (..)

import Html exposing (Attribute, Html, div, h1, h2, h3, p, span, text)
import Html.Attributes exposing (class, style)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Dialog exposing (dialog)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(..))
import SecureVote.SPAs.SwarmMVP.Views.SwmAddressV exposing (swmAddressV)
import SecureVote.SPAs.SwarmMVP.Views.SwmCastVoteV exposing (castVoteView)
import SecureVote.SPAs.SwarmMVP.Views.SwmVotingV exposing (votingView)


rootView : Model -> Html Msg
rootView model =
    slideHost model
        [ ( SwmAddressR, swmAddressV model )
        , ( SwmVoteR, castVoteView model )
        , ( SwmSubmitR, votingView model )
        ]
        [ dialog model ]


slideHost : Model -> List ( Route, Html Msg ) -> List (Html Msg) -> Html Msg
slideHost model slideParis extraHtml =
    let
        currSlide =
            model.route

        slideOutCs route =
            if route == List.head model.history ? NotFoundR then
                "slide-out"
            else
                ""

        drawSlide ( route, slide ) =
            if route == currSlide then
                div [ class "w-100 slider slide-in" ] [ slide ]
            else if List.member route model.history then
                div [ class <| "w-100 slider " ++ slideOutCs route ] [ slide ]
            else
                div [] []

        slides =
            List.map drawSlide slideParis
    in
    div [ class "w-100" ] (slides ++ extraHtml)
