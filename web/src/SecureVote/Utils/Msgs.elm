module SecureVote.Utils.Msgs exposing (..)


msgOrErr : (a -> msg) -> (String -> msg) -> Result String a -> msg
msgOrErr msg failMsg r =
    case r of
        Ok r ->
            msg r

        Err err ->
            failMsg err
