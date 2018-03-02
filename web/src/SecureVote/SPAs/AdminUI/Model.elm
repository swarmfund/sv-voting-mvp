module SecureVote.SPAs.AdminUI.Model exposing (..)

import Dict exposing (Dict)
import Element.Input exposing (SelectWith)
import Json.Decode exposing (Value)
import Json.Encode exposing (object)
import SecureVote.Ballots.Types exposing (BallotSpec, BallotSpecChoice, OptsChoice, emptyBSpec01)
import SecureVote.Crypto.Hashing exposing (HashModel)
import SecureVote.SPAs.AdminUI.Fields exposing (democHashId)
import SecureVote.SPAs.AdminUI.Msg exposing (Msg)
import SecureVote.SPAs.AdminUI.Types exposing (Flags)


type alias Model =
    { strFields : Dict String String
    , boolFields : Dict String Bool
    , select : Maybe (SelectWith BallotSpecChoice Msg)
    , selectOpts : Maybe (SelectWith OptsChoice Msg)
    , workingBallot : BallotSpec
    , savedBallots : Dict String BallotSpec
    , jsonBallot : String
    , hash : HashModel
    , mainTitle : String
    , dev : Bool
    , errors : List String
    , web3 : Web3Model
    , indexAddr : String
    , indexABI : String
    , log : List String
    }


initModel : Flags -> Model
initModel { mainTitle, dev, democHash, indexAddr, indexABI } =
    { strFields = Dict.fromList [ ( democHashId, democHash ) ]
    , boolFields = Dict.empty
    , select = Nothing
    , selectOpts = Nothing
    , workingBallot = emptyBSpec01
    , savedBallots = Dict.empty
    , jsonBallot = "Choose a ballot type"
    , hash = "0x"
    , mainTitle = mainTitle
    , dev = dev
    , errors = []
    , web3 = initWeb3Model
    , indexAddr = indexAddr
    , indexABI = indexABI
    , log = []
    }


type alias Web3Model =
    { txid : Maybe String
    }


initWeb3Model =
    { txid = Nothing
    }
