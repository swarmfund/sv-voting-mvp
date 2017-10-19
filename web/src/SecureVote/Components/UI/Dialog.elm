module SecureVote.Components.UI.Dialog exposing (..)

import Html exposing (Attribute, Html, div, h1, text)
import Html.Attributes exposing (class)
import Material.Dialog as Dialog
import Material.Options as Options exposing (cs, css)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
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
                    gethDialogV

                VerifyDialog ->
                    verifyDialogV

                DebugDialog ->
                    debugDialogV model

                NotFoundDialog ->
                    h1 [ class "red" ] [ text "Not Found" ]
    in
    Dialog.view
        -- Tachyons has no Max-Height :(
        -- possible flex attrs: flex flex-column justify-between
        [ cs "overflow-scroll w-75"
        , css "max-height" "75%"
        ]
        [ Dialog.title [] [ text model.dialogHtml.title ]
        , Dialog.content [ cs "" ]
            [ innerHtml
            ]
        , Dialog.actions [ cs "v-btm" ]
            [ btn 976565675 model [ SecBtn, CloseDialog, Attr (class "fr") ] [ text "Close" ]
            ]
        ]
