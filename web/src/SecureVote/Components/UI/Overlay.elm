module SecureVote.Components.UI.Overlay exposing (..)

import Element exposing (Element, el)
import Element.Attributes exposing (spacing)
import SecureVote.Components.UI.CommonStyles exposing (CommonStyle(GreyOut, NoS), cmnSpacing)


greyOut : (CommonStyle -> s) -> Element s v m -> Element s v m
greyOut w e =
    el (w GreyOut) [ spacing cmnSpacing ] <| e
