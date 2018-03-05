module SecureVote.SPAs.DelegationUI.Views.Styles exposing (..)

import Color exposing (black, darkGray, red)
import Element exposing (Element)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg)
import Style exposing (prop, style, styleSheet, variation)
import Style.Border exposing (..)
import Style.Color exposing (border, text)
import Style.Font exposing (font, monospace, size, typeface)


type alias UiElem =
    Element DelegationStyles Variations Msg


type DelegationStyles
    = NoS
    | Title
    | ErrTxt
    | BallotElemSelector
    | TxPreview
    | PayloadHash
    | Field
    | SubMenu
    | TxRender


type Variations
    = NoVar
    | RoundedTop
    | RoundedAll
    | Disabled


radius =
    5


stylesheet =
    styleSheet
        [ style NoS []
        , style BallotElemSelector []
        , style Title
            [ text black
            , size 30
            ]
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
        , style TxRender
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
            , variation Disabled
                [ prop "disabled" "disabled" ]
            ]
        , style SubMenu
            [ border black
            , all 1
            , roundBottomRight radius
            , roundBottomLeft radius
            ]
        ]
