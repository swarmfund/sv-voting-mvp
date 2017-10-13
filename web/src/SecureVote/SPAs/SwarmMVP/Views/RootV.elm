module SecureVote.SPAs.SwarmMVP.Views.RootV exposing (..)

import Dict
import Html exposing (Attribute, Html, div, h1, h2, h3, p, span, text)
import Html.Attributes exposing (class, style)
import Material.Button as Button
import Material.Options as Options exposing (css, cs)
import Material.Scheme
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Textfield as Textf
import Material.Typography exposing (display1, display2, display3, headline, title)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..))
import Maybe.Extra exposing ((?))
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route(..))
import SecureVote.SPAs.SwarmMVP.Views.SwmAddressV exposing (swmAddressV)


rootView : Model -> Html Msg
rootView model =
    slideHost model
        [ ( SwmAddressR, swmAddressV model )
        , ( SwmVoteR, h1 [] [ text "Slide #2!" ] )
        ]
        |> Material.Scheme.top


secView : Model -> Html Msg
secView model =
    div
        []
        [ swmAddressV model

        {- We construct the instances of the Button component that we need, one
           for the increase button, one for the reset button. First, the increase
           button. The first three arguments are:
             - A Msg constructor (`Mdl`), lifting Mdl messages to the Msg type.
             - An instance id (the `[0]`). Every component that uses the same model
               collection (model.mdl in this file) must have a distinct instance id.
             - A reference to the elm-mdl model collection (`model.mdl`).
           Notice that we do not have to add fields for the increase and reset buttons
           separately to our model; and we did not have to add to our update messages
           to handle their internal events.
           Mdl components are configured with `Options`, similar to `Html.Attributes`.
           The `Options.onClick Increase` option instructs the button to send the `Increase`
           message when clicked. The `css ...` option adds CSS styling to the button.
           See `Material.Options` for details on options.
        -}
        --        , Button.render Mdl
        --            [ 0 ]
        --            model.mdl
        --            [ Options.onClick Increase
        --            , css "margin" "0 24px"
        --            ]
        --            [ text "Increase" ]
        --        , Button.render Mdl
        --            [ 1 ]
        --            model.mdl
        --            [ Options.onClick Reset ]
        --            [ text "Reset" ]
        ]
        |> Material.Scheme.top


slideHost : Model -> List ( Route, Html Msg ) -> Html Msg
slideHost model slideParis =
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
            else
                div [ class <| "w-100 slider " ++ slideOutCs route ] [ slide ]

        slides =
            List.map drawSlide slideParis
    in
        div [ class "w-100" ] slides
