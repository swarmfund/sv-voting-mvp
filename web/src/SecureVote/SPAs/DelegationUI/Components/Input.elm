module SecureVote.SPAs.DelegationUI.Components.Input exposing (..)

import Element.Attributes exposing (..)
import Element.Input as I
import SecureVote.Components.UI.CommonStyles exposing (Variations(..))
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(SelectTokenContract))
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (DelegationStyles(..))


stdPad =
    10


stdSpace =
    0


select =
    I.select Field [ padding stdPad, spacing stdSpace, vary RoundedTop True, maxWidth <| percent 100 ]


radio =
    I.radioRow NoS []


genDropSelect =
    I.dropMenu Nothing SelectTokenContract


genAutoComplete =
    I.autocomplete Nothing SelectTokenContract


textInput =
    I.text Field [ padding stdPad, spacing stdSpace, vary RoundedAll True ]


textArea =
    I.multiline Field [ padding stdPad, spacing stdSpace, vary RoundedAll True, minHeight <| px 75 ]


checkbox =
    I.checkbox Field [ padding stdPad, spacing stdSpace, vary RoundedAll True ]
