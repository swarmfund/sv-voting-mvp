module Test.SV.Encryption where 



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
import SecureVote.Utils.ArrayBuffer (fromHex, toHex, ui8FromArray)
import Test.SV.Types (SpecType)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)


anEncTest :: forall e. BoxKeyPair -> BoxKeyPair -> Message -> Boolean
anEncTest keypairVoter keypairEnc toEnc = do
        let voterPk = getBoxPublicKey keypairVoter
        let voterSk = getBoxSecretKey keypairVoter
        let encPk = getBoxPublicKey keypairEnc
        let encSk = getBoxSecretKey keypairEnc 
        let encd = encryptOneTimeBallot voterPk toEnc encPk voterSk
        let decdM = decryptOneTimeBallot encd voterPk encSk
        case decdM of
            Nothing -> false
            (Just decd) -> do 
                let msgHex = toHex $ toUint8Array toEnc
                let decryptedHex = toHex $ toUint8Array decd
                msgHex == decryptedHex


qcEncTest :: forall e. BoxKeyPair -> BoxKeyPair -> Array Int -> Boolean
qcEncTest keypairVoter keypairEnc msg = do 
        -- note, calculating msgTrimmed isn't actually necessary - ui8FromArray trims them anyway
        let msgTrimmed = map (abs <<< flip mod 256) msg
        anEncTest keypairVoter keypairEnc (toMessage $ ui8FromArray msgTrimmed)


encTests :: forall e. SpecType e
encTests = do
    it "should decrypt things we encrypt - trivial case" do
        {keypairVoter, keypairEnc} <- genKeyPairs
        let msg1 = unsafePartial $ fromJust $ toMessage <$> fromHex "deadbeef"
        shouldEqual true $ anEncTest keypairVoter keypairEnc msg1
    it "should decrypt things quickcheck provides" do
        {keypairVoter, keypairEnc} <- genKeyPairs
        quickCheck $ qcEncTest keypairVoter keypairEnc
    where
      genKeyPairs = do
        keypairVoter <- liftEff $ genCurve25519Key
        keypairEnc <- liftEff $ genCurve25519Key
        pure $ {keypairVoter, keypairEnc}
        