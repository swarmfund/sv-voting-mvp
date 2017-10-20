module SecureVote.List.Utils exposing (..)


padLeft : Int -> a -> List a -> List a
padLeft len thing things =
    if List.length things >= len then
        things
    else
        padLeft len thing (thing :: things)


padRight : Int -> a -> List a -> List a
padRight len thing things =
    if List.length things >= len then
        things
    else
        padRight len thing (things ++ [ thing ])
