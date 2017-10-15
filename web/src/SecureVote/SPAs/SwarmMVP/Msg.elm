module SecureVote.SPAs.SwarmMVP.Msg exposing (..)

import Material
import SecureVote.Eth.Models exposing (CandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route)


type Msg
    = SetElevation Int Bool
    | SetField String String
    | ChangePage Route
    | SetCandidateTx (CandidateEthTx -> CandidateEthTx)
    | MultiMsg (List Msg)
      -- Elm Mdl
    | Mdl (Material.Msg Msg)
