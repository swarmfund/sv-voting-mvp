module Test.SV.Encryption where 



import Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Crypt.NaCl (getBoxPublicKey, getBoxSecretKey, toUint8Array)
import Data.Foreign.Keys (keys)
import Data.Maybe (fromJust, isNothing)
import Partial.Unsafe (unsafePartial)
import SecureVote.Crypto.Curve25519 (genCurve25519Key, toMessage, decryptOneTimeBallot, encryptOneTimeBallot)
import SecureVote.Utils.ArrayBuffer (fromHex, toHex)
import Test.SV.Types (SpecType)
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)



encTests :: forall e. SpecType e
encTests = do
    it "should decrypt things we encrypt" do 
        keypairVoter <- liftEff $ genCurve25519Key
        keypairEnc <- liftEff $ genCurve25519Key
        let voterPk = getBoxPublicKey keypairVoter
        let voterSk = getBoxSecretKey keypairVoter
        let encPk = getBoxPublicKey keypairEnc
        let encSk = getBoxSecretKey keypairEnc 
        let msg = unsafePartial $ fromJust $ toMessage <$> fromHex "deadbeef"
        let encd = encryptOneTimeBallot voterPk msg encPk voterSk
        let decdM = decryptOneTimeBallot encd voterPk encSk
        shouldEqual (isNothing decdM) false 
        let decd = unsafePartial $ fromJust decdM 
        let msgHex = toHex $ toUint8Array msg
        let decryptedHex = toHex $ toUint8Array decd
        shouldEqual msgHex decryptedHex
        log msgHex
        log decryptedHex        
        
    
