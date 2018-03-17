module SecureVote.SmartContracts.Erc20 exposing (balanceOf)

import Decimal exposing (Decimal)
import Json.Decode as Decode
import Json.Encode exposing (list, string)
import Maybe.Extra
import SecureVote.Eth.Tasks exposing (decodeAndUnpackResp, readContract)
import Task


erc20ABI : String
erc20ABI =
    """[{"constant":true,"inputs":[{"name":"_owner","type":"address"}],"name":"balanceOf","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"}]"""


balanceOf : { scAddr : String, addr : String, atBlock : String } -> Task.Task String Decimal
balanceOf { scAddr, addr, atBlock } =
    readContract
        { addr = scAddr
        , abi = erc20ABI
        , args = list <| [ string addr, string atBlock ]
        , method = "balanceOf"
        }
        |> decodeAndUnpackResp Decode.string
        |> Task.andThen (Decimal.fromString >> Maybe.Extra.unpack (\_ -> Task.fail "unable to deserialize balance to decimal") Task.succeed)
        |> Task.map (Debug.log "balanceOf returns:")
