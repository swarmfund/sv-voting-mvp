module SecureVote.Components.UI.Dialog exposing (..)

import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (class)
import Material.Dialog as Dialog
import Material.Options as Options exposing (cs)
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(Mdl))


dialog : Model -> Html Msg
dialog model =
    Dialog.view
        -- Tachyons has no Max-Height :(
        -- possible flex attrs: flex flex-column justify-between
        [ cs "overflow-scroll w-75 h-75"
        ]
        [ Dialog.title [] [ text model.dialogHtml.title ]
        , Dialog.content [ cs "" ]
            [ model.dialogHtml.html
            ]
        , Dialog.actions []
            [ btn 976565675 model [ SecBtn, CloseDialog, Attr (class "fr") ] [ text "Close" ]
            ]
        ]
