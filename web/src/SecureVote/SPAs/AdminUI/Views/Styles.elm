module SecureVote.SPAs.AdminUI.Views.Styles exposing (..)

import Color exposing (black, darkGray)
import Element exposing (Element)
import SecureVote.Components.UI.CommonStyles exposing (CommonStyle, Variations(..), commonStyleSheet)
import SecureVote.SPAs.AdminUI.Msg exposing (Msg)
import Style exposing (prop, style, styleSheet, variation)
import Style.Border exposing (..)
import Style.Color exposing (border, text)
import Style.Font exposing (font, monospace, size, typeface)


type alias UiElem =
    Element AdminStyles Variations Msg


type AdminStyles
    = NoS
    | Title
    | BallotElemSelector
    | BallotPreview
    | BallotHash
    | Field
    | SubMenu
    | BallotRender
    | Code
    | LogEntry
    | EventLogBox
    | CS CommonStyle


radius =
    5


stylesheet =
    styleSheet <|
        [ style NoS []
        , style BallotElemSelector []
        , style Title
            [ text black
            , size 30
            ]
        , style BallotPreview
            [ border black
            , rounded 5
            , all 1
            ]
        , style BallotHash
            [ border black
            , rounded 5
            , all 1
            , typeface [ monospace ]
            ]
        , style BallotRender
            [ typeface [ monospace ]
            ]
        , style Field
            [ border black
            , all 1
            , variation RoundedTop
                [ roundTopRight radius
                , roundTopLeft radius
                ]
            , variation RoundedAll
                [ rounded radius ]
            ]
        , style SubMenu
            [ border black
            , all 1
            , roundBottomRight radius
            , roundBottomLeft radius
            ]
        , style Code
            [ typeface [ monospace ]
            , variation FSmall
                [ size 12
                ]
            ]
        , style LogEntry
            [ bottom 0.5
            , border darkGray
            , dashed
            ]
        , style EventLogBox
            [ all 1
            , rounded radius
            , border black
            ]
        ]
            ++ commonStyleSheet CS
