module SecureVote.Components.UI.Collapsible exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import SecureVote.Components.UI.CommonStyles exposing (..)


collapsible w { onCollapse, isCollapsed, header, body } =
    let
        collapsedSymbol =
            if isCollapsed then
                "➕"
            else
                "➖"

        collapsedSection =
            if isCollapsed then
                []
            else
                body
    in
    column (w Collapsible)
        [ spacing cmdSpacing ]
    <|
        [ el (w Title) [ onClick onCollapse, vary BtnCursor True ] <| text <| collapsedSymbol ++ " " ++ header
        ]
            ++ collapsedSection
