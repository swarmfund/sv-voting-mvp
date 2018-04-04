module SV.Light.SmartContract where

import SV.Prelude

import SV.Light.Types.RunBallot (SmartContract, TxOpts)
import SecureVote.Web3.Web3 (runWeb3_)


mkSC :: forall args e a. TxOpts -> SmartContract e args a
mkSC tos f c args = eToAff <=< eToAff <=< runWeb3_ $ f tos c args
