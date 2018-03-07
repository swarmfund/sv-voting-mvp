module SecureVote.Tokens.Types exposing (..)


type TokenContract
    = Swarm


tcChoiceToStr : TokenContract -> String
tcChoiceToStr c =
    case c of
        Swarm ->
            "SWM (0x9e88613418cF03dCa54D6a2cf6Ad934A78C7A17A)"


tcChoiceToAddr : Maybe TokenContract -> String
tcChoiceToAddr c =
    case c of
        Just Swarm ->
            "0x9e88613418cF03dCa54D6a2cf6Ad934A78C7A17A"

        _ ->
            "Unknown Contract"
