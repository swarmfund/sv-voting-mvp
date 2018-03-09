module SecureVote.Eth.Update exposing (..)

import SecureVote.Eth.Model exposing (..)
import SecureVote.Eth.Msg exposing (..)
import SecureVote.Eth.Types exposing (..)
import SecureVote.Eth.Web3 exposing (..)


ethUpdate : EthMsg -> EthMdl -> ( EthMdl, Cmd EthMsg )
ethUpdate msg model =
    case msg of
        WriteViaMM doc ->
            model ! [ performContractWriteMM doc ]

        ReadContract doc ->
            model ! [ performContractRead doc ]
