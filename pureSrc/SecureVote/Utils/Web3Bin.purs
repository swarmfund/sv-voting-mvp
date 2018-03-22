module SecureVote.Utils.Web3Bin where

import SV.Prelude

import Network.Ethereum.Web3 (BytesN, HexString, unBytesN)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize)
import SecureVote.Utils.Binary (bsToHexStr)

bytesNToHex :: forall n. KnownSize n => BytesN n -> HexString
bytesNToHex = bsToHexStr <<< unBytesN
