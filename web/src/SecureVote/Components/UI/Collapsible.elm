module SecureVote.Components.UI.Collapsible exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import SecureVote.Components.UI.CommonStyles exposing (..)


type alias CollapsibleOpts s msg =
    { onCollapse : msg
    , isCollapsed : Bool
    , header : String
    , body : List (Element s Variations msg)
    , startOpen : Bool
    , smallTitle : Bool
    }


collapsible : (CommonStyle -> s) -> CollapsibleOpts s msg -> Element s Variations msg
collapsible w { onCollapse, isCollapsed, header, body, startOpen, smallTitle } =
    let
        isOpen =
            xor startOpen isCollapsed

        collapsedSymbol =
            if not isOpen then
                "➕"
            else
                "➖"

        titleSty =
            if smallTitle then
                SubTitle
            else
                Title
    in
    column (w Collapsible)
        [ spacing cmnSpacing ]
    <|
        [ el (w titleSty) [ onClick onCollapse, vary BtnCursor True ] <| text <| collapsedSymbol ++ " " ++ header
        ]
            ++ [ when isOpen <| column (w NoS) [ spacing cmnSpacing ] body ]
