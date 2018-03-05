module SecureVote.SPAs.DelegationUI.Model exposing (..)

import Dict exposing (Dict)
import Element.Input exposing (SelectWith)
import Json.Decode exposing (Value)
import Json.Encode exposing (object)
import SecureVote.SPAs.DelegationUI.Components.Input exposing (genDropSelect)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg)
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationTx, DelegationType(Global), Flags, TokenContractChoice)


type alias Model =
    { strFields : Dict String String
    , boolFields : Dict String Bool
    , tokenConAddr : SelectWith TokenContractChoice Msg
    , delType : Maybe DelegationType
    , delTx : DelegationTx
    , mainTitle : String
    , dev : Bool
    , democHash : String
    , errors : List String
    , littleGovIndex : String
    , delABI : String
    , delConAddr : String
    }


initModel : Flags -> Model
initModel { mainTitle, dev, democHash, indexAddr, delegationABI } =
    { strFields = Dict.empty
    , boolFields = Dict.empty
    , tokenConAddr = genDropSelect
    , delType = Just Global
    , delTx = DelegationTx "" "" 0 200000 ""
    , mainTitle = mainTitle
    , dev = dev
    , democHash = democHash
    , errors = []
    , littleGovIndex = indexAddr
    , delABI = delegationABI
    , delConAddr = "0xd78D4beAbFD3054390D10aeb4258dC2D867f5e17"
    }


type alias Web3Model =
    { txid : Maybe String
    }


initWeb3Model =
    { txid = Nothing
    }
