module SecureVote.Components.UI.Typo exposing (..)

import Html exposing (span, text)
import Material.Options as Options exposing (cs)
import Material.Typography as T


cCs =
    "pt3-ns pt3-m pt2 pb3-ns pb3-m pb2"


headline txt =
    Options.styled span [ cs <| "black db f2-ns f3-m f3 fw2 lh-title " ++ cCs ] [ text txt ]


subhead txt =
    Options.styled span [ cs <| "black db f3-ns f4-m f4 fw2 lh-title " ++ cCs ] [ text txt ]
