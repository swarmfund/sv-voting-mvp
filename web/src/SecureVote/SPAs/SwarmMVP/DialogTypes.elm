module SecureVote.SPAs.SwarmMVP.DialogTypes exposing (..)

import Html exposing (Html, div, em, p, text)


type DialogHtml msg
    = DlogP (List (DialogHtml msg))
    | DlogEm String
    | DlogTxt String
    | DlogDiv (List (DialogHtml msg))
      -- Dialog with classes or whatever
    | DlogWAttr (List (Html.Attribute msg)) (DialogHtml msg)


dialogHtmlRender : List (Html.Attribute msg) -> DialogHtml msg -> Html msg
dialogHtmlRender attrs input =
    let
        renderedElems elems =
            List.map (dialogHtmlRender []) elems
    in
    case input of
        DlogDiv elems ->
            div attrs <| renderedElems elems

        DlogP elems ->
            p attrs <| renderedElems elems

        DlogEm str ->
            em attrs [ text str ]

        DlogTxt str ->
            text str

        DlogWAttr attrs dlogHtml ->
            dialogHtmlRender attrs dlogHtml
