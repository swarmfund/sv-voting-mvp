module SecureVote.Components.UI.Typo exposing (..)

import Html exposing (span, text)
import Material.Options as Options exposing (cs)
import Material.Typography as T


headline txt =
    Options.styled span [ cs "black db f2 pt3 pb4 fw2 lh-title" ] [ text txt ]


subhead txt =
    Options.styled span [ cs "black db f3 fw2 pv3 lh-title" ] [ text txt ]
