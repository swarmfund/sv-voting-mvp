module SV.Utils.BigNumber where

import SV.Prelude

import Network.Ethereum.Web3 (BigNumber, decimal, embed)
import Network.Ethereum.Web3 as BN

bnFromMDef0 :: Maybe BigNumber -> BigNumber
bnFromMDef0 = fromMaybe (embed 0)

bnToStr :: BigNumber -> String
bnToStr = BN.toString decimal
