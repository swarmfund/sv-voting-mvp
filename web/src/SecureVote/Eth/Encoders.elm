module SecureVote.Eth.Encoders exposing (..)

import Json.Encode exposing (Value, object, string)
import SecureVote.Eth.Models exposing (MinEthTx)


minEthTxEncoder : MinEthTx -> Value
minEthTxEncoder minTx =
    object
        [ ( "from", string minTx.from )
        , ( "to", string minTx.to )
        , ( "value", string minTx.value )
        , ( "data", string minTx.data )
        ]
