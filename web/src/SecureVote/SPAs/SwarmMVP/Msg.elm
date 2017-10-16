module SecureVote.SPAs.SwarmMVP.Msg exposing (..)

import Html exposing (Html)
import Material
import SecureVote.Eth.Models exposing (CandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route)


type Msg
    = SetElevation Int Bool
    | SetField String String
    | ChangePage Route
    | SetDialog (Html Msg)
    | SetCandidateTx (CandidateEthTx -> CandidateEthTx)
    | MultiMsg (List Msg)
      -- Elm Mdl
    | Mdl (Material.Msg Msg)
