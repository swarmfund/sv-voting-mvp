module SecureVote.Components.UI.Dialog exposing (..)

import Html exposing (Attribute, Html, div, h1, h3, hr, span, text)
import Html.Attributes exposing (class, style)
import Material.Dialog as Dialog
import Material.Icon as MIcon
import Material.Options as Options exposing (cs, css)
import SecureVote.Components.UI.Btn as Btn exposing (BtnProps(..), btn)
import SecureVote.Components.UI.Typo exposing (headline)
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
                    infoDialogV model

                GethDialog ->
                    gethDialogV model

                VerifyDialog ->
                    verifyDialogV model

                FullAuditDialog ->
                    fullAuditDialogV model

                NotFoundDialog ->
                    h1 [ class "red" ] [ text "Not Found" ]

                MEWDialog ->
                    mewDialog model
    in
    Dialog.view
        -- Tachyons has no Max-Height :(
        -- possible flex attrs: flex flex-column justify-between
        [ cs "w-80 w-70-l flex flex-column pa0"
        , css "max-height" "80%"
        , Options.id "dialog-container"
        ]
        -- span here fixes graphical error on safari
        [ Dialog.title
            -- f7 needed as font size of dialog title is buggering things up
            [ cs "f7 ma0 pa0" ]
            [ div [ class "flex flex-row mt3 mh4" ]
                [ div [ class "flex-auto" ] [ headline model.dialogHtml.title ]
                , btn 384394893 model [ Btn.Icon, CloseDialog, Attr (class "mt1") ] [ MIcon.view "close" [ MIcon.size24 ] ]
                ]
            , hr [ class "mb0 w-100" ] []
            ]
        , Dialog.content
            [ cs "overflow-y-scroll db flex-auto mh4" ]
            [ div
                [ class "db " ]
                [ innerHtml ]
            ]
        , Dialog.actions [ cs "v-btm ma0 pa0 f7 flex flex-column" ]
            [ hr [ class "ma0 w-100", style [ ( "height", "0" ) ] ] []
            , div [ class "flex flex-row mb4 mh4 mt3" ]
                [ span [ class "flex-auto" ] []
                , btn 976565675 model [ SecBtn, CloseDialog, Attr (class "") ] [ text "Close" ]
                ]
            ]
        ]
