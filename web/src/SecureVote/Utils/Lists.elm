module SecureVote.Utils.Lists exposing (..)

import List.Extra


enumerate : List a -> List ( Int, a )
enumerate xs =
    List.Extra.zip (List.range 0 (List.length xs)) xs
