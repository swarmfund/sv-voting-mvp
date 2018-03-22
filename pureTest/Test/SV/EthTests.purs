module Test.SV.EthTests where


import SV.Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as EffC
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Crypt.NaCl (BoxKeyPair(..), Message, NACL_RANDOM, getBoxPublicKey, getBoxSecretKey, toUint8Array)
import Data.Foreign.Keys (keys)
import Data.Int (decimal, toStringAs)
import Data.Lens (view)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Ord (abs)
import Data.String (joinWith)
import Network.Ethereum.Web3 (ChainCursor(..), embed, unsafeToInt)
import Network.Ethereum.Web3.Api (eth_getBlockByNumber)
import Partial.Unsafe (unsafePartial)
import SV.Light.AuditBallot (findEthBlockEndingInZeroBefore)
import SecureVote.Crypto.Curve25519 (genCurve25519Key, toMessage, decryptOneTimeBallot, encryptOneTimeBallot)
import SecureVote.Democs.SwarmMVP.BallotContract (getBlockTimestamp, setWeb3Provider)
import SecureVote.Utils.ArrayBuffer (fromHex, toHex, ui8FromArray)
import SecureVote.Web3.Web3 (EthNetwork(..), runWeb3Dev, runWeb3Prod, setNetwork)
import Test.SV.Types (SpecType)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)



ethTests :: forall e. SpecType e
ethTests = do
    it "should find blocks reliably" do
        -- web3URL, web3Auth
        setWeb3Provider "https://mainnet.eth.secure.vote:8545" ""
        setNetwork Mainnet
        -- run the block test 3 times for random blocks
        runBlockTest
        runBlockTest
        runBlockTest


runBlockTest :: Aff _ Unit
runBlockTest = do
    blockZero <- map unwrap $ eToAff =<< (runWeb3Prod $ eth_getBlockByNumber (BN $ wrap $ embed 1))  -- can't use `Earliest` because ts == 0
    blockLatest <- map unwrap $ eToAff =<< (runWeb3Prod $ eth_getBlockByNumber Latest)
    log $ "ts:" <> show (unsafeToInt blockZero.timestamp) <> "," <> show (unsafeToInt blockLatest.timestamp)
    targetBlockTs <- liftEff $ randomInt (unsafeToInt blockZero.timestamp) (unsafeToInt blockLatest.timestamp)
    foundBlockN <- findEthBlockEndingInZeroBefore targetBlockTs
    let nextBlockN = foundBlockN + 1
    blockTs <- getBlockTimestamp foundBlockN
    blockNextTs <- getBlockTimestamp nextBlockN
    (blockTs <= targetBlockTs && targetBlockTs < blockNextTs) `shouldEqual` true
