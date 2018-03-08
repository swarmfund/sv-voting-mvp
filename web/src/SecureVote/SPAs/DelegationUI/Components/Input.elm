module SecureVote.SPAs.DelegationUI.Components.Input exposing (..)

import Element exposing (Element)
import Element.Attributes exposing (..)
import Element.Input as I
import SecureVote.Components.UI.CommonStyles exposing (CommonStyle(..), Variations(..))
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(SelectTokenContract))


stdPad =
    10


stdSpace =
    0


select w =
    I.select (w Field) [ padding stdPad, spacing stdSpace, vary RoundedTop True, maxWidth <| percent 100 ]


radio : (CommonStyle -> s) -> I.Radio o s v msg -> Element s v msg
radio w =
    I.radioRow (w NoS) []


genDropSelect =
    I.dropMenu Nothing SelectTokenContract


genAutoComplete =
    I.autocomplete Nothing SelectTokenContract


textInput w =
    I.text (w Field) [ padding stdPad, spacing stdSpace, vary RoundedAll True ]


textArea w =
    I.multiline (w Field) [ padding stdPad, spacing stdSpace, vary RoundedAll True, minHeight <| px 75 ]


checkbox w =
    I.checkbox (w Field) [ padding stdPad, spacing stdSpace, vary RoundedAll True ]
