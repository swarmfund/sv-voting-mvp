module SecureVote.Components.UI.Btn exposing (..)

import Element as E exposing (Element, button)
import Element.Attributes exposing (padding)
import Element.Events exposing (onClick)
import Html exposing (Attribute, Html, div, h1, h2, h3, p, span, text)
import Html.Attributes exposing (class, style, target)
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Options as Options exposing (cs, css)
import SecureVote.Components.UI.CommonStyles exposing (CommonStyle(..), Variations, cmnPad)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(Mdl))


type BtnProps
    = PriBtn
    | SecBtn
    | Flat
    | Icon
    | Click Msg
    | Link String
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

                Link url ->
                    ( [], [ Button.link url, Options.attribute <| target "_blank" ] )

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


type alias ButtonOpts msg =
    { text : String
    , onClick : msg
    }


cmnBtn : (CommonStyle -> s) -> ButtonOpts msg -> Element s Variations msg
cmnBtn w opts =
    button (w Field) [ padding cmnPad, onClick opts.onClick ] (E.text opts.text)
