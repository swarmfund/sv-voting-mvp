module Test.SV.EthTests where


import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as EffC
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Crypt.NaCl (BoxKeyPair(..), Message, NACL_RANDOM, getBoxPublicKey, getBoxSecretKey, toUint8Array)
import Data.Foreign.Keys (keys)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Ord (abs)
import Data.String (joinWith)
import Partial.Unsafe (unsafePartial)
import SecureVote.Crypto.Curve25519 (genCurve25519Key, toMessage, decryptOneTimeBallot, encryptOneTimeBallot)
import SecureVote.Democs.SwarmMVP.BallotContract (findEthBlockEndingInZeroBefore, getBlockTimestamp, setWeb3Provider)
import SecureVote.Utils.ArrayBuffer (fromHex, toHex, ui8FromArray)
import Test.SV.Types (SpecType)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)



ethTests :: forall e. SpecType e
ethTests = do
    it "should find blocks reliably" do
        setWeb3Provider "https://eth-aws-nv-node-02.secure.vote:8545" ""
        -- run the block test 3 times for random blocks
        runBlockTest
        runBlockTest
        runBlockTest


runBlockTest :: Aff _ Unit
runBlockTest = do
    targetBlockTs <- liftEff $ randomInt 1438269988 1518486687 -- timestamp between block 1 and block 5080184
    foundBlockN <- findEthBlockEndingInZeroBefore targetBlockTs
    let nextBlockN = foundBlockN + 1
    blockTs <- getBlockTimestamp foundBlockN
    blockNextTs <- getBlockTimestamp nextBlockN
    (blockTs <= targetBlockTs && targetBlockTs < blockNextTs) `shouldEqual` true
