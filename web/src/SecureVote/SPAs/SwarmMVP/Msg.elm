module SecureVote.SPAs.SwarmMVP.Msg exposing (..)

import Decimal exposing (Decimal)
import Html exposing (Html)
import Material
import SecureVote.Eth.Models exposing (CandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route)


type
    Msg
    -- UI Maintenance msgs
    = SetElevation Int Bool
    | SetField String String
    | PageGoForward Route
    | PageGoBack
    | SetDialog String (Html Msg)
    | SetBallotRange Int Float
      -- Eth related msgs
    | SetCandidateTx (CandidateEthTx -> CandidateEthTx)
    | SetEthNode String
      -- Utility msgs
    | MultiMsg (List Msg)
      -- Port msgs
    | ToWeb3 ToWeb3Msg
    | FromWeb3 FromWeb3Msg
      -- Errors
    | LogErr String
      -- Elm Mdl
    | Mdl (Material.Msg Msg)


type ToWeb3Msg
    = SetProvider
    | GetErc20Balance


type FromWeb3Msg
    = GotBalance Decimal
