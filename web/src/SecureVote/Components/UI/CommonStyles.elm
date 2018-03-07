module SecureVote.Components.UI.CommonStyles exposing (..)

import Color exposing (..)
import Style exposing (Property, Style, StyleSheet, cursor, style, variation)
import Style.Color exposing (text)
import Style.Font exposing (..)


cmnPad =
    10


cmdSpacing =
    10


type alias RenderInputType s =
    List ( CommonStyle, List (Property s Variations) )


renderSS : (CommonStyle -> s) -> RenderInputType s -> List (Style s Variations)
renderSS w ps =
    List.map (\( s, props ) -> style (w s) <| props) ps


type CommonStyle
    = CodeStyle
    | Warning
    | Collapsible
    | Title
    | SubTitle
    | SubSubTitle
    | NoS


type Variations
    = BtnCursor
    | NoVar
    | RoundedTop
    | RoundedAll
    | Disabled


commonStyleSheet : (CommonStyle -> s) -> List (Style s Variations)
commonStyleSheet wrap =
    renderSS wrap
        [ ( NoS, [] )
        , ( CodeStyle
          , [ typeface [ monospace ]
            ]
          )
        , ( Warning
          , [ size 20
            , text red
            ]
          )
        , ( Title
          , [ text black
            , size 30
            , variation BtnCursor [ cursor "pointer" ]
            ]
          )
        , ( SubTitle
          , [ text black
            , size 20
            ]
          )
        , ( SubSubTitle
          , [ text black
            , size 15
            ]
          )
        ]
