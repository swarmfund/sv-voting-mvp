module SecureVote.Components.UI.CommonStyles exposing (..)

import Color exposing (..)
import Style exposing (Property, Style, StyleSheet, cursor, prop, style, variation)
import Style.Border exposing (..)
import Style.Color exposing (background, border, text)
import Style.Font exposing (..)


cmnPad =
    20


cmnSpacing =
    20


cmnRound =
    5


type alias RenderInputType s =
    List ( CommonStyle, List (Property s Variations) )


renderSS : (CommonStyle -> s) -> RenderInputType s -> List (Style s Variations)
renderSS w ps =
    List.map (\( s, props ) -> style (w s) <| props) ps


type CommonStyle
    = CodeStyle
    | Warning
    | Collapsible
    | PageTitle
    | Title
    | SubTitle
    | SubSubTitle
    | Field
    | NoS
    | LoadingIndStyle
    | GreyOut
    | LinProcElement


type Variations
    = BtnCursor
    | NoVar
    | RoundedTop
    | RoundedAll
    | Disabled
    | FSmall


commonStyleSheet : (CommonStyle -> s) -> List (Style s Variations)
commonStyleSheet wrap =
    renderSS wrap
        [ ( NoS, [] )
        , ( CodeStyle
          , [ typeface [ monospace ]
            ]
          )
        , ( Warning
          , [ size 17
            , text red
            , background <| rgb 255 200 200
            , border red
            , left 2
            ]
          )
        , ( PageTitle
          , [ text black
            , size 40
            , bold
            ]
          )
        , ( Title
          , [ text black
            , size 30
            , variation BtnCursor [ cursor "pointer" ]
            , bold
            ]
          )
        , ( SubTitle
          , [ text black
            , size 20
            , bold
            ]
          )
        , ( SubSubTitle
          , [ text black
            , size 17
            , bold
            ]
          )
        , ( Field
          , [ border black
            , all 1
            , variation RoundedTop
                [ roundTopRight cmnRound
                , roundTopLeft cmnRound
                ]
            , variation RoundedAll
                [ rounded cmnRound ]
            , variation Disabled
                [ prop "disabled" "disabled" ]
            ]
          )
        , ( Collapsible, [] )
        , ( LoadingIndStyle, [ size 25 ] )
        , ( GreyOut
          , []
          )
        , ( LinProcElement
          , [ top 1
            , border black
            ]
          )
        ]
