module SecureVote.SPAs.DelegationUI.Model exposing (..)

import Dict exposing (Dict)
import Element.Input exposing (SelectWith)
import Json.Decode exposing (Value)
import Json.Encode exposing (object)
import RemoteData exposing (RemoteData(NotAsked))
import SecureVote.Eth.Model exposing (EthMdl, initEthMdl)
import SecureVote.Eth.Nodes exposing (ethNodes)
import SecureVote.SPAs.DelegationUI.Components.Input exposing (genAutoComplete, genDropSelect)
import SecureVote.SPAs.DelegationUI.Msg exposing (Msg)
import SecureVote.SPAs.DelegationUI.Types exposing (..)
import SecureVote.Tokens.Types exposing (TokenContract)


type alias Model =
    { strFields : Dict String String
    , boolFields : Dict String Bool
    , tokenConAddr : SelectWith TokenContract Msg
    , delType : Maybe DelegationType
    , delTx : DelegationTx
    , mainTitle : String
    , dev : Bool
    , errors : List String
    , delegationABI : String
    , delegationAddr : String
    , viewDlgtResp : RemoteData String DelegationResp
    , eth : EthMdl
    }


initModel : Flags -> Model
initModel { mainTitle, dev, delegationABI, delegationAddr, mmDetected } =
    let
        ethNode_ =
            if dev then
                ethNodes.kovan
            else
                ethNodes.mainnet
    in
    { strFields = Dict.empty
    , boolFields = Dict.empty
    , tokenConAddr = genAutoComplete
    , delType = Just Global
    , delTx = DelegationTx "" "" 0 ""
    , mainTitle = mainTitle
    , dev = dev
    , errors = []
    , delegationABI = delegationABI
    , delegationAddr = delegationAddr
    , viewDlgtResp = NotAsked
    , eth = initEthMdl { mmDetected = mmDetected, ethNode = ethNode_ }
    }


type alias Web3Model =
    { txid : Maybe String
    }


initWeb3Model =
    { txid = Nothing
    }
