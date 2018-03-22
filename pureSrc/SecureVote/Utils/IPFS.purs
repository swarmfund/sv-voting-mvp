module SecureVote.Utils.IPFS where

import SV.Prelude

import Network.Ethereum.Web3 (HexString)

foreign import hexHashToSha256Bs58 :: HexString -> String
