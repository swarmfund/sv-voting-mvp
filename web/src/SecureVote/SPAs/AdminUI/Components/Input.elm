module SecureVote.SPAs.AdminUI.Components.Input exposing (..)

import Element.Attributes exposing (minHeight, padding, px, spacing, vary)
import Element.Input as I
import SecureVote.Components.UI.CommonStyles exposing (Variations(..))
import SecureVote.SPAs.AdminUI.Msg exposing (Msg(SelectBallot))
import SecureVote.SPAs.AdminUI.Views.Styles exposing (AdminStyles(..))


stdPad =
    10


stdSpace =
    0


select =
    I.select Field [ padding stdPad, spacing stdSpace, vary RoundedTop True ]


genDropSelect =
    I.dropMenu Nothing SelectBallot


textInput =
    I.text Field [ padding stdPad, spacing stdSpace, vary RoundedAll True ]


textArea =
    I.multiline Field [ padding stdPad, spacing stdSpace, vary RoundedAll True, minHeight <| px 75 ]


checkbox =
    I.checkbox Field [ padding stdPad, spacing stdSpace, vary RoundedAll True ]
