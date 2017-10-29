module SecureVote.Components.UI.Dialog exposing (..)

import Html exposing (Attribute, Html, div, h1, h3, span, text)
import Html.Attributes exposing (class, style)
import Material.Dialog as Dialog
import Material.Icon as MIcon
import Material.Options as Options exposing (cs, css)
import SecureVote.Components.UI.Btn as Btn exposing (BtnProps(..), btn)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(Mdl))
import SecureVote.SPAs.SwarmMVP.Routes exposing (DialogRoute(..))
import SecureVote.SPAs.SwarmMVP.Views.DialogV exposing (..)


dialog : Model -> Html Msg
dialog model =
    let
        innerHtml =
            case model.dialogHtml.route of
                SettingsDialog ->
                    settingsDialogV model

                BallotDialog content ->
                    customDialogV content

                InfoDialog ->
                    infoDialogV

                GethDialog ->
                    gethDialogV model

                VerifyDialog ->
                    verifyDialogV model

                NotFoundDialog ->
                    h1 [ class "red" ] [ text "Not Found" ]

                MEWDialog ->
                    mewDialog model
    in
    Dialog.view
        -- Tachyons has no Max-Height :(
        -- possible flex attrs: flex flex-column justify-between
        [ cs "w-80 w-70-l"
        , cs "overflow-scroll"
        , css "max-height" "80%"
        , Options.id "dialog-container"
        ]
        -- span here fixes graphical error on safari
        [ Dialog.title
            [ cs "" ]
            [ div []
                [ h3 [ class "mv0 dib" ] [ text model.dialogHtml.title ]
                , btn 384394893 model [ Btn.Icon, CloseDialog, Attr (class "fr") ] [ MIcon.view "close" [ MIcon.size24 ] ]
                ]
            ]
        , Dialog.content
            [ cs "overflow-y-scroll db" ]
            [ div
                [ class "db " ]
                [ innerHtml ]
            ]
        , Dialog.actions [ cs "v-btm mb3" ]
            [ btn 976565675 model [ SecBtn, CloseDialog, Attr (class "fr") ] [ text "Close" ]
            ]
        ]
