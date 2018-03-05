module SecureVote.SPAs.DelegationUI.Types exposing (..)


type alias Flags =
    { mainTitle : String, dev : Bool, democHash : String, indexAddr : String, indexABI : String, delegationABI : String }


type alias DelegationTx =
    { to : String
    , from : String
    , value : Int
    , gas : Int
    , data : String
    }


type DelegationType
    = Global
    | Token


type TokenContractChoice
    = Swarm


tcChoiceToStr : TokenContractChoice -> String
tcChoiceToStr c =
    case c of
        Swarm ->
            "Swarm Token"


tcChoiceToAddr : Maybe TokenContractChoice -> String
tcChoiceToAddr c =
    case c of
        Just Swarm ->
            --        TODO: Update Swarm Contract Address
            "0x23452345"

        _ ->
            ""
