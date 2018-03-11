module SecureVote.Eth.Subscriptions exposing (..)

import SecureVote.Eth.Msg exposing (EthMsg(GotWeb3))
import SecureVote.Eth.Web3 exposing (gotWeb3)


ethGenericSubs w =
    Sub.batch
        [ gotWeb3 (w << GotWeb3)
        ]
