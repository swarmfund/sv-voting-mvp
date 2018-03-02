module SecureVote.SPAs.DelegationUI.Components.Input exposing (..)

import Element.Attributes exposing (minHeight, padding, px, spacing, vary)
import Element.Input as I
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg(SelectBallot))
import SecureVote.SPAs.DelegationUI.Views.Styles exposing (AdminStyles(..), Variations(..))


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
