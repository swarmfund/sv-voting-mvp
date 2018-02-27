module SecureVote.SPAs.AdminUI.Views.Render exposing (..)

import Element exposing (Element, column, el, text)
import Element.Attributes exposing (fill, height, padding, px, spacing, vary, width)
import Json.Encode exposing (encode)
import SecureVote.SPAs.AdminUI.Model exposing (Model)
import SecureVote.SPAs.AdminUI.Msg exposing (Msg)
import SecureVote.SPAs.AdminUI.Views.Styles exposing (AdminStyles(..), UiElem, Variations(..))


renderBallotSpec : Model -> UiElem
renderBallotSpec model =
    column BallotPreview
        [ padding 20 ]
        [ el BallotRender [] (text model.jsonBallot) ]


renderBallotHash : Model -> UiElem
renderBallotHash model =
    el BallotHash [ padding 20, height <| px 60 ] (text model.sha3)
