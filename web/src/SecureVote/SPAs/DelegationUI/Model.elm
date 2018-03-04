module SecureVote.SPAs.DelegationUI.Model exposing (..)

import Dict exposing (Dict)
import Element.Input exposing (SelectWith)
import Json.Decode exposing (Value)
import Json.Encode exposing (object)
import SecureVote.Ballots.Types exposing (BallotSpec, BallotSpecChoice, OptsChoice, emptyBSpec01)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg)
import SecureVote.SPAs.DelegationUI.Types exposing (DelegationTx, Flags)


type alias Model =
    { strFields : Dict String String
    , boolFields : Dict String Bool
    , select : Maybe (SelectWith BallotSpecChoice Msg)
    , selectOpts : Maybe (SelectWith OptsChoice Msg)
    , delegationTx : DelegationTx
    , mainTitle : String
    , dev : Bool
    , democHash : String
    , errors : List String
    , web3 : Web3Model
    , littleGovIndex : String
    , delegationABI : String
    , contractAddr : String
    }


initModel : Flags -> Model
initModel { mainTitle, dev, democHash, indexAddr, delegationABI } =
    { strFields = Dict.empty
    , boolFields = Dict.empty
    , select = Nothing
    , selectOpts = Nothing
    , delegationTx = DelegationTx "" "" 0 200000 ""
    , mainTitle = mainTitle
    , dev = dev
    , democHash = democHash
    , errors = []
    , web3 = initWeb3Model
    , littleGovIndex = indexAddr
    , delegationABI = delegationABI
    , contractAddr = "0xd78D4beAbFD3054390D10aeb4258dC2D867f5e17"
    }


type alias Web3Model =
    { txid : Maybe String
    }


initWeb3Model =
    { txid = Nothing
    }
