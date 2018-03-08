module SecureVote.SPAs.DelegationUI.Views.Styles exposing (..)

import Color exposing (black, darkGray, red)
import Element exposing (Element)
import SecureVote.Components.UI.CommonStyles exposing (CommonStyle, Variations(..), commonStyleSheet)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg)
import Style exposing (prop, style, styleSheet, variation)
import Style.Border exposing (..)
import Style.Color exposing (border, text)
import Style.Font exposing (font, monospace, size, typeface)


type alias UiElem =
    Element DelegationStyles Variations Msg


type DelegationStyles
    = DNoS
    | ErrTxt
    | BallotElemSelector
    | TxPreview
    | PayloadHash
    | SubMenu
    | TxRender
    | CS CommonStyle


radius =
    5


stylesheet =
    styleSheet <|
        [ style DNoS []
        , style BallotElemSelector []
        , style ErrTxt
            [ text red ]
        , style TxPreview
            [ border black
            , rounded 5
            , all 1
            ]
        , style PayloadHash
            [ border black
            , rounded 5
            , all 1
            , typeface [ monospace ]
            ]
        , style SubMenu
            [ border black
            , all 1
            , roundBottomRight radius
            , roundBottomLeft radius
            ]
        ]
            ++ commonStyleSheet CS
