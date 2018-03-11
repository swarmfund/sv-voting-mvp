module SecureVote.SPAs.DelegationUI.Types exposing (..)


type alias Flags =
    { mainTitle : String
    , dev : Bool
    , democHash : String
    , delegationABI : String
    , delegationAddr : String
    , mmDetected : Bool
    }


type alias DelegationTx =
    { to : String
    , from : String
    , value : Int
    , data : String
    }


type DelegationType
    = Global
    | Token


type alias DelegationResp =
    { dId : Int
    , prevDId : Int
    , setAtBlock : Int
    , delegatee : String
    , delegator : String
    , tokenAddr : String
    }



{- pairs are (voter, tokenContract); global delecation has 0x00 addr for tokenContract -}


type alias DelegatorsResp =
    List { voterAddr : String, tokenAddr : String }
