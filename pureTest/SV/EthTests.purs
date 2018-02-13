module Test.SV.EthTests where


import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as EffC
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Crypt.NaCl (BoxKeyPair(..), Message, NACL_RANDOM, getBoxPublicKey, getBoxSecretKey, toUint8Array)
import Data.Foreign.Keys (keys)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Ord (abs)
import Data.String (joinWith)
import Partial.Unsafe (unsafePartial)
import SecureVote.Crypto.Curve25519 (genCurve25519Key, toMessage, decryptOneTimeBallot, encryptOneTimeBallot)
import SecureVote.Democs.SwarmMVP.BallotContract (findEthBlockEndingInZeroBefore, setWeb3Provider)
import SecureVote.Utils.ArrayBuffer (fromHex, toHex, ui8FromArray)
import Test.SV.Types (SpecType)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)



ethTests :: forall e. SpecType e
ethTests = do
    it "should find blocks reliably" do
        setWeb3Provider "https://eth-aws-nv-node-02.secure.vote:8545" ""
        -- pass in block numbers and their respective timestamps or the timestamps + 1
        runBlockTest 5004580 1517386389
        runBlockTest 4004580 1499719488
        runBlockTest 4501330 1509973295


runBlockTest :: Int -> Int -> Aff _ Unit
runBlockTest targetBlockN targetBlockTs = do
    foundBlockN <- findEthBlockEndingInZeroBefore targetBlockTs
    targetBlockN `shouldEqual` foundBlockN
