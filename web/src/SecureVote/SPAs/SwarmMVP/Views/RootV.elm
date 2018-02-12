module SecureVote.SPAs.SwarmMVP.Views.RootV exposing (..)

import Html exposing (Attribute, Html, div, h1, h2, h3, img, p, span, text)
import Html.Attributes exposing (attribute, class, id, src, style)
import Material.Snackbar as Snackbar
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Dialog exposing (dialog)
import SecureVote.SPAs.SwarmMVP.Model exposing (LastPageDirection(PageForward), Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(..))
import SecureVote.SPAs.SwarmMVP.Views.BallotOpeningSlideV exposing (openingSlide)
import SecureVote.SPAs.SwarmMVP.Views.CastVoteV exposing (castVoteView)
import SecureVote.SPAs.SwarmMVP.Views.HowToVoteV exposing (howToVoteView)
import SecureVote.SPAs.SwarmMVP.Views.ListVotesV exposing (listVotesView)
import SecureVote.SPAs.SwarmMVP.Views.SetAddressV exposing (swmAddressV)
import SecureVote.SPAs.SwarmMVP.Views.SetDelegateV exposing (delegateView)
import SecureVote.SPAs.SwarmMVP.Views.VotingV exposing (votingView)


rootView : Model -> Html Msg
rootView model =
    slideHost model
        [ ( SwmAddressR, swmAddressV model )
        , ( SwmHowToVoteR, howToVoteView model )
        , ( SwmVoteR, castVoteView model )
        , ( SwmDelegateR, delegateView model )
        , ( SwmSubmitR, votingView model )
        , ( OpeningSlideR, openingSlide model )
        , ( ListAllVotesR, listVotesView model )
        ]
        [ img [ src "img/swarm-logo-white-sm.png", class "mv1 mh4-l center db" ] []
        , dialog model
        , Snackbar.view model.snack |> Html.map Snackbar
        ]


slideHost : Model -> List ( Route, Html Msg ) -> List (Html Msg) -> Html Msg
slideHost model slideParis extraHtml =
    let
        currSlide =
            model.route

        slideOutCs route =
            if route == List.head model.history ? NotFoundR then
                if model.lastPageDirection == PageForward then
                    "slide-out"
                else
                    ""
            else if Just route == model.lastRoute then
                "slide-out-back"
            else
                ""

        slideInCs =
            if model.lastPageDirection == PageForward then
                "slide-in"
            else
                "slide-in-back"

        joinCs =
            String.join " "

        commonCs =
            "w-100 slider"

        drawSlide ( route, slide ) =
            div [ attribute "data-sv-slide" <| toString route, class "" ]
                [ if route == currSlide then
                    div [ class <| joinCs [ commonCs, slideInCs ] ] [ slide ]
                  else if List.member route <| List.take 1 model.history then
                    div [ class <| joinCs [ commonCs, slideOutCs route ] ] [ slide ]
                  else if model.lastRoute == Just route then
                    div [ class <| joinCs [ commonCs, slideOutCs route ] ] [ slide ]
                  else
                    div [] []
                ]

        slides =
            List.map drawSlide slideParis
    in
    div [ class "w-100", id "sv-main" ] (slides ++ extraHtml)
