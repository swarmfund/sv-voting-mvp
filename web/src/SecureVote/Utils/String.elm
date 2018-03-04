module SecureVote.Utils.String exposing (..)


chunk : Int -> String -> List String
chunk n s =
    let
        chunk_ n s ss =
            case s of
                "" ->
                    ss

                _ ->
                    chunk_ n (String.slice n (String.length s) s) (String.slice 0 n s :: ss)
    in
    chunk_ n s []
