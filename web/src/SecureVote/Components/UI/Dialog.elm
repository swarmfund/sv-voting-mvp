module SecureVote.Components.UI.Dialog exposing (..)

import Html exposing (Attribute, Html, text)
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
        [ cs "h-75 overflow-scroll"
        ]
        [ Dialog.title [] [ text "Greetings" ]
        , Dialog.content []
            [ model.dialogHtml
            ]
        , Dialog.actions []
            -- Both Buttons do the same thing.
            [ btn 976565675 model [ PriBtn, CloseDialog ] [ text "Okay" ]
            , btn 458567467 model [ SecBtn, CloseDialog ] [ text "Close" ]
            ]
        ]
