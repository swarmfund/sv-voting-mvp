module SecureVote.SPAs.AdminUI.Model exposing (..)

import Dict exposing (Dict)
import Element.Input exposing (SelectWith)
import Json.Decode exposing (Value)
import Json.Encode exposing (object)
import SecureVote.Ballots.LittleGovIndexes exposing (getIndex, littleGovIndexs)
import SecureVote.Ballots.Types exposing (BallotSpec, BallotSpecChoice, OptsChoice, emptyBSpec01)
import SecureVote.SPAs.AdminUI.Msg exposing (Msg)


type alias Model =
    { strFields : Dict String String
    , boolFields : Dict String Bool
    , select : Maybe (SelectWith BallotSpecChoice Msg)
    , selectOpts : Maybe (SelectWith OptsChoice Msg)
    , workingBallot : BallotSpec
    , savedBallots : Dict String BallotSpec
    , jsonBallot : String
    , sha3 : String
    , mainTitle : String
    , dev : Bool
    , democHash : String
    , errors : List String
    , web3 : Web3Model
    , littleGovIndex : String
    }


initModel : String -> Bool -> String -> Model
initModel mainTitle dev democHash =
    { strFields = Dict.empty
    , boolFields = Dict.empty
    , select = Nothing
    , selectOpts = Nothing
    , workingBallot = emptyBSpec01
    , savedBallots = Dict.empty
    , jsonBallot = "{}"
    , sha3 = "0x"
    , mainTitle = mainTitle
    , dev = dev
    , democHash = democHash
    , errors = []
    , web3 = initWeb3Model
    , littleGovIndex = getIndex dev
    }


type alias Web3Model =
    { txid : Maybe String
    }


initWeb3Model =
    { txid = Nothing
    }
