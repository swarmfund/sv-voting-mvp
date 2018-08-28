module SecureVote.SPAs.AdminUI.Model exposing (..)

import Dict exposing (Dict)
import Element.Input exposing (SelectWith)
import Json.Decode exposing (Value)
import Json.Encode exposing (object)
import RemoteData exposing (RemoteData)
import SecureVote.Ballots.Types exposing (BallotSpec, BallotSpecChoice, OptsChoice, emptyBSpec01)
import SecureVote.Const exposing (archivePushProdURL, archivePushTestURL, fakeErc20Addr)
import SecureVote.Crypto.Hashing exposing (HashModel)
import SecureVote.SPAs.AdminUI.Fields exposing (democHashId, erc20Id, rangeVoteNumOptsId)
import SecureVote.SPAs.AdminUI.Msg exposing (Msg)
import SecureVote.SPAs.AdminUI.Types exposing (Flags)


type alias Model =
    { strFields : Dict String String
    , intFields : Dict String Int
    , boolFields : Dict String Bool
    , loadingFields : Dict String (RemoteData String String)
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
    , archivePushApiKey : String
    , archivePushURL : String
    }


initModel : Flags -> Model
initModel { mainTitle, dev, democHash, indexAddr, indexABI, archivePushApiKey } =
    let
        initStrFields =
            [ ( democHashId, democHash ) ]
                ++ (if dev then
                        [ ( erc20Id, fakeErc20Addr ) ]
                    else
                        [ ( erc20Id, "0x9e88613418cf03dca54d6a2cf6ad934a78c7a17a" ) ]
                   )

        ( archivePushURL, meh ) =
            if dev then
                ( archivePushTestURL, "" )
            else
                ( archivePushProdURL, "" )
    in
    { strFields = Dict.fromList initStrFields
    , intFields = Dict.fromList [ ( rangeVoteNumOptsId, 1 ) ]
    , boolFields = Dict.empty
    , loadingFields = Dict.empty
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
    , archivePushApiKey = archivePushApiKey
    , archivePushURL = archivePushURL
    }


type alias Web3Model =
    { txid : Maybe String
    , txInfo : String
    }


initWeb3Model =
    { txid = Nothing
    , txInfo = "Nothing yet.."
    }
