module SecureVote.Components.UI.StatusMsgs exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import SecureVote.Components.UI.CommonStyles exposing (..)


warning w msg =
    el (w Warning) [ padding cmnPad ] (text msg)
