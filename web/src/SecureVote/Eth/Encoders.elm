module SecureVote.Eth.Encoders exposing (..)

import Json.Encode exposing (Value, int, object, string)
import SecureVote.Eth.Types exposing (..)


minEthTxEncoder : MinEthTx -> Value
minEthTxEncoder minTx =
    object
        [ ( "from", string minTx.from )
        , ( "to", string minTx.to )
        , ( "value", int minTx.value )
        , ( "data", string minTx.data )
        , ( "gas", string minTx.gas )
        ]
