module SecureVote.Components.UI.Btn exposing (..)

import Dict
import Html exposing (Attribute, Html, div, h1, h2, h3, p, span, text)
import Html.Attributes exposing (class, style)
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Dialog as Dialog
import Material.Elevation as Elevation
import Material.Options as Options exposing (cs, css)
import Material.Scheme
import Material.Textfield as Textf
import Material.Typography exposing (display1, display2, display3, headline, title)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(Mdl))


type BtnProps
    = PriBtn
    | SecBtn
    | Flat
    | Icon
    | Click Msg
    | Disabled
    | OpenDialog
    | CloseDialog
    | BtnNop -- doesn't do anything
    | Opt (Button.Property Msg)
    | Attr (Html.Attribute Msg)


btn : Int -> Model -> List BtnProps -> List (Html Msg) -> Html Msg
btn id model props inner =
    let
        btnPropToAttr prop =
            case prop of
                PriBtn ->
                    ( [], [ Button.colored, Button.raised, Button.ripple ] )

                SecBtn ->
                    ( [], [ Button.plain, Button.raised, Button.ripple ] )

                Flat ->
                    ( [], [ Button.flat, Button.ripple ] )

                Icon ->
                    ( [], [ Button.icon ] )

                Click msg ->
                    ( [], [ Options.onClick msg ] )

                Disabled ->
                    ( [], [ Button.disabled ] )

                BtnNop ->
                    ( [], [] )

                OpenDialog ->
                    ( [], [ Dialog.openOn "click" ] )

                CloseDialog ->
                    ( [], [ Dialog.closeOn "click" ] )

                Opt opt ->
                    ( [], [ opt ] )

                Attr attr ->
                    ( [ attr ], [] )

        f btnProp ( attrs, opts ) =
            let
                ( newAttrs, newOpts ) =
                    btnPropToAttr btnProp
            in
            ( attrs ++ newAttrs, opts ++ newOpts )

        ( attrs, opts ) =
            List.foldl f ( [ class "dib" ], [] ) props
    in
    div attrs
        [ Button.render Mdl [ id ] model.mdl opts inner
        ]
