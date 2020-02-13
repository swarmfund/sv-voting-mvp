module SecureVote.Tokens.Types exposing (..)


type TokenContract
    = Swarm


tcChoiceToStr : TokenContract -> String
tcChoiceToStr c =
    case c of
        Swarm ->
            "SWM (0x3505f494c3f0fed0b594e01fa41dd3967645ca39)"


tcChoiceToAddr : Maybe TokenContract -> String
tcChoiceToAddr c =
    case tcChoiceToAddrM c of
        Just a ->
            a

        _ ->
            "No contract selected"


tcChoiceToAddrM : Maybe TokenContract -> Maybe String
tcChoiceToAddrM c =
    case c of
        Just Swarm ->
            Just "0x3505f494c3f0fed0b594e01fa41dd3967645ca39"

        _ ->
            Nothing
