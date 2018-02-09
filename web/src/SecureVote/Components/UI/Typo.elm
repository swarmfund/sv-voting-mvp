module SecureVote.Components.UI.Typo exposing (..)

import Html exposing (span, text)
import Material.Options as Options exposing (cs)
import Material.Typography as T


headline txt =
    Options.styled span [ T.headline, cs "black db" ] [ text txt ]
