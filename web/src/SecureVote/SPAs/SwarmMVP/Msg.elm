module SecureVote.SPAs.SwarmMVP.Msg exposing (..)

import Html exposing (Html)
import Material
import SecureVote.Eth.Models exposing (CandidateEthTx)
import SecureVote.SPAs.SwarmMVP.Routes exposing (Route)


type Msg
    = SetElevation Int Bool
    | SetField String String
    | ChangePage Route
    | ChangeToPreviousPage
    | SetDialog (Html Msg)
    | SetBallotRange Int Float
    | SetCandidateTx (CandidateEthTx -> CandidateEthTx)
    | MultiMsg (List Msg)
    | SetEthNode String
    | UpdateTokenBalance
      -- Elm Mdl
    | Mdl (Material.Msg Msg)
