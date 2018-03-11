module SecureVote.Eth.Update exposing (..)

import Maybe.Extra
import SecureVote.Eth.Model exposing (..)
import SecureVote.Eth.Msg exposing (..)
import SecureVote.Eth.Tasks exposing (ethTasksInit)
import SecureVote.Eth.Utils exposing (isValidEthAddress)
import SecureVote.Eth.Web3 exposing (..)
import Task


type alias Mdl a =
    { a | eth : EthMdl }


ethUpdate : (EthMsg -> m) -> EthMsg -> Mdl a -> ( Mdl a, Cmd m )
ethUpdate lift msg model =
    let
        ( m, c ) =
            case msg of
                EthNop ->
                    model ! []

                WriteViaMM doc ->
                    model ! [ performContractWriteMM doc ]

                ReadContract doc ->
                    model ! [ performContractRead doc ]

                RefreshMMAddress ->
                    model ! [ getMMAddress () ]

                SetMMAddress a ->
                    let
                        m_ =
                            model.eth

                        mmAddr =
                            Just a |> Maybe.Extra.filter isValidEthAddress
                    in
                    { model | eth = { m_ | mmAddr = mmAddr } } ! []

                SetEthProvider p ->
                    let
                        m_ =
                            model.eth
                    in
                    { model | eth = { m_ | ethNode = p } } ! [ initWeb3WProvider p ]

                GotWeb3 v ->
                    model ! [ Task.perform (\_ -> EthNop) (ethTasksInit v) ]
    in
    ( m, Cmd.map lift c )
