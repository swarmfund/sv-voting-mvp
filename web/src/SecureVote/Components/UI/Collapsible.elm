module SecureVote.Components.UI.Collapsible exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import SecureVote.Components.UI.CommonStyles exposing (..)


collapsible w { onCollapse, isCollapsed, header, body, startOpen } =
    let
        collapsed_ =
            not <| xor startOpen isCollapsed

        collapsedSymbol =
            if collapsed_ then
                "➕"
            else
                "➖"

        collapsedSection =
            if collapsed_ then
                []
            else
                body
    in
    column (w Collapsible)
        [ spacing cmnSpacing ]
    <|
        [ el (w Title) [ onClick onCollapse, vary BtnCursor True ] <| text <| collapsedSymbol ++ " " ++ header
        ]
            ++ collapsedSection
