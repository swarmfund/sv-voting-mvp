module SecureVote.Components.UI.Dialog exposing (..)

import Html exposing (Attribute, Html, div, h1, h3, span, text)
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
                    gethDialogV model

                VerifyDialog ->
                    verifyDialogV

                DebugDialog ->
                    debugDialogV model

                NotFoundDialog ->
                    h1 [ class "red" ] [ text "Not Found" ]

                MEWDialog ->
                    mewDialog model
    in
    Dialog.view
        -- Tachyons has no Max-Height :(
        -- possible flex attrs: flex flex-column justify-between
        [ cs "w-75 w-50-l"
        , css "max-height" "80%"
        , css "top" "10%"
        , cs "overflow-auto"
        , Options.id "dialog-container"
        ]
        -- span here fixes graphical error on safari
        [ Dialog.title [] [ h3 [ class "mv0" ] [ text model.dialogHtml.title ] ]
        , Dialog.content [] [ div [ class "overflow-scroll" ] [ innerHtml ] ]
        , Dialog.actions [ cs "v-btm" ]
            [ btn 976565675 model [ SecBtn, CloseDialog, Attr (class "fr") ] [ text "Close" ]
            ]
        ]
