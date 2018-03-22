module SV.Light.Types.BallotBox where

-- | # Ballot Box Smart Contract Related Types

import Network.Ethereum.Web3 (Address, HexString)

-- | # Ballot Box Smart Contract Related Types

type BallotFromSC = {i :: Int, voterAddr :: Address, voterPk :: HexString, ballot :: HexString}
