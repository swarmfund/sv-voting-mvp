module SecureVote.SPAs.SwarmMVP.Views.SettingsDialogV exposing (..)

import Html exposing (Html, div, span, text)
import Material.Options as Options exposing (cs, css)
import Material.Textfield as Textf
import Material.Typography exposing (menu)
import Maybe.Extra exposing ((?))
import SecureVote.Components.UI.Btn exposing (BtnProps(..), btn)
import SecureVote.SPAs.SwarmMVP.Helpers exposing (getEthNodeTemp, setEthNodeTemp)
import SecureVote.SPAs.SwarmMVP.Model exposing (Model, initModel)
import SecureVote.SPAs.SwarmMVP.Msg exposing (Msg(..), ToWeb3Msg(SetProvider))


settingsDialogV : Model -> Html Msg
settingsDialogV model =
    let
        setEthNodeMsgs =
            MultiMsg
                [ SetEthNode <| getEthNodeTemp model ? ""
                , ToWeb3 SetProvider
                ]
    in
    div []
        [ div []
            [ Options.styled span [ menu, cs "" ] [ text "Ethereum Node URL" ]
            , Textf.render Mdl
                [ 7785646743 ]
                model.mdl
                [ Options.onInput <| setEthNodeTemp
                , Textf.value <| getEthNodeTemp model ? ""

                -- Seems to be an issue with the label not disappearing properly.
                --                , Textf.label initModel.ethNode
                , cs "mh4"
                , css "min-width" "400px"
                ]
                []
            , btn 456467568 model [ PriBtn, Click setEthNodeMsgs ] [ text "Update" ]
            ]
        ]
