module SecureVote.Web3.Web3
    ( EthNetwork(..)
    , setNetwork
    , runWeb3_
    , runWeb3Dev
    , runWeb3Prod
    , runWeb3Classic
    , zeroHash
    , zeroAddr
    , setProvider
    ) where

import SV.Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Network.Ethereum.Web3 (Address, ETH, Provider, Web3Error, httpProvider, mkAddress, mkHexString, runWeb3)
import Network.Ethereum.Web3.Types (Web3)
import Network.Ethereum.Web3.Types.Types (HexString)
import Partial.Unsafe (unsafePartial)

upmhx :: String -> HexString
upmhx = unsafePartial fromJust <<< mkHexString

zeroHash :: HexString
zeroHash = upmhx "0x0000000000000000000000000000000000000000000000000000000000000000"
zeroAddr :: Address
zeroAddr = unsafePartial fromJust $ mkAddress $ upmhx "0x0000000000000000000000000000000000000000"


data EthNetwork
    = Mainnet
    | Kovan
    | Classic
    | Ropsten
    | OtherNet String


svMainnetProvider :: Provider
svMainnetProvider = unsafePerformEff $ httpProvider "https://mainnet.eth.secure.vote:8545"

svKovanProvider :: Provider
svKovanProvider = unsafePerformEff $ httpProvider "https://kovan.eth.secure.vote:8545"

svClassicProvider :: Provider
svClassicProvider = unsafePerformEff $ httpProvider "https://classic.eth.secure.vote:8545"

svRopstenProvider :: Provider
svRopstenProvider = unsafePerformEff $ httpProvider "https://ropsten.eth.secure.vote:8545"


_getNet :: EthNetwork -> Provider
_getNet n = case n of
    Mainnet -> svMainnetProvider
    Kovan -> svKovanProvider
    Classic -> svClassicProvider
    Ropsten -> svRopstenProvider
    OtherNet p -> unsafePerformEff $ httpProvider p


setNetwork :: forall e. EthNetwork -> Aff (ref :: REF | e) Unit
setNetwork n =  liftEff $ writeRef _svNetVar n


setProvider :: forall e. String -> Eff (ref :: REF | e) Unit
setProvider p = writeRef _svNetVar $ OtherNet p


_svNetVar :: Ref EthNetwork
_svNetVar = unsafePerformEff $ newRef Mainnet


runWeb3Prod :: forall e a. Web3 e a -> Aff (eth :: ETH | e) (Either Web3Error a)
runWeb3Prod = runWeb3 svMainnetProvider

runWeb3Dev :: forall e a. Web3 e a -> Aff (eth :: ETH | e) (Either Web3Error a)
runWeb3Dev = runWeb3 svKovanProvider

runWeb3Classic :: forall e a. Web3 e a -> Aff (eth :: ETH | e) (Either Web3Error a)
runWeb3Classic = runWeb3 svClassicProvider

runWeb3Ropsten :: forall e a. Web3 e a -> Aff (eth :: ETH | e) (Either Web3Error a)
runWeb3Ropsten = runWeb3 svClassicProvider

runWeb3_ :: forall e eff a. Web3 (ref :: REF | e) a -> Aff (eth :: ETH, ref :: REF | e) (Either Web3Error a)
runWeb3_ w3r = do
    net <- liftEff $ readRef _svNetVar
    runWeb3 (_getNet net) w3r
