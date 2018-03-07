module SecureVote.Components.UI.Typo exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Html as H
import Material.Options as Options exposing (cs)
import Material.Typography as T
import SecureVote.Components.UI.CommonStyles exposing (..)


cCs =
    "pt3-ns pt3-m pt2 pb3-ns pb3-m pb2"


headline txt =
    Options.styled H.span [ cs <| "black db f2-ns f3-m f3 fw2 lh-title " ++ cCs ] [ H.text txt ]


subhead txt =
    Options.styled H.span [ cs <| "black db f3-ns f4-m f4 fw2 lh-title " ++ cCs ] [ H.text txt ]


title w msg =
    el (w Title) [] (text msg)


subtitle w msg =
    el (w SubTitle) [] (text msg)
