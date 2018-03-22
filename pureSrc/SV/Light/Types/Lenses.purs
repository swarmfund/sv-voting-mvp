module SV.Types.Lenses where

import SV.Prelude

import Data.Lens (Lens', lens)
import Network.Ethereum.Web3 (Address, HexString)
import SV.Light.Types.Ballot (BSpec01Impl, BallotSpec(..), OptsOuter)


getFromBSpec :: forall a. (BSpec01Impl -> a) -> BallotSpec -> a
getFromBSpec gV01 b = case b of
    BVer01 bs -> gV01 bs


setFromBSpec :: forall a. (BSpec01Impl -> a -> BSpec01Impl) -> BallotSpec -> a -> BallotSpec
setFromBSpec sV01 b thing = case b of
    BVer01 bs -> BVer01 $ sV01 bs thing


_erc20Addr :: Lens' BallotSpec Address
_erc20Addr = lens
    (getFromBSpec (\r -> r.erc20Addr))
    (setFromBSpec (\bs addr -> bs {erc20Addr = addr}))


_endTime :: Lens' BallotSpec Int
_endTime = lens
    (getFromBSpec (\r -> r.endTime))
    (setFromBSpec (\bs et -> bs {endTime = et}))

_encryptionPK :: Lens' BallotSpec (Maybe HexString)
_encryptionPK = lens
    (getFromBSpec (\r -> r.encryptionPK))
    (setFromBSpec (\r encpk -> r {encryptionPK = encpk}))

_options :: Lens' BallotSpec OptsOuter
_options = lens
    (getFromBSpec (\r -> r.options))
    (setFromBSpec (\r opts -> r {options = opts}))
