module SecureVote.Crypto.Curve25519 where
  
import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Crypt.NaCl (Box, BoxPublicKey, BoxSecretKey, Message, Nonce, toUint8Array)
import Crypt.NaCl.Box (generateBoxKeyPair, box, boxOpen)
import Crypt.NaCl.Hash (hash)
import Data.Array (slice)
import Data.ArrayBuffer.Typed (toArray, toIntArray)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Function.Uncurried (Fn1, Fn3)
import Data.Int (fromNumber)
import Data.Maybe (Maybe)
import Data.TypedArray (asUint8Array)
import SecureVote.Crypto.NativeEd25519 (sha256)
import SecureVote.Utils.ArrayBuffer (toHex)
import Unsafe.Coerce (unsafeCoerce)


toMessage :: Uint8Array -> Message
toMessage = unsafeCoerce

toBoxPubkey :: Uint8Array -> BoxPublicKey
toBoxPubkey = unsafeCoerce

toBoxSeckey :: Uint8Array -> BoxSecretKey
toBoxSeckey = unsafeCoerce

toNonce :: Uint8Array -> Nonce
toNonce = unsafeCoerce

toBox :: Uint8Array -> Box
toBox = unsafeCoerce


genCurve25519Key = generateBoxKeyPair


genOneTimeNonce :: BoxPublicKey -> Nonce
genOneTimeNonce senderPk = toNonce $ asUint8Array $ slice 0 24 $ toIntArray $ sha256 $ toUint8Array senderPk


decryptOneTimeBallot :: Box -> BoxPublicKey -> BoxSecretKey -> Maybe Message
decryptOneTimeBallot boxToOpen voterPk ballotEncSk = boxOpen boxToOpen (genOneTimeNonce voterPk) voterPk ballotEncSk 


encryptOneTimeBallot :: BoxPublicKey -> Message -> BoxPublicKey -> BoxSecretKey -> Box
encryptOneTimeBallot senderPk msg encPk senderSk = box msg (genOneTimeNonce senderPk) encPk senderSk
