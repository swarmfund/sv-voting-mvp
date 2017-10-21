module SecureVote.Crypto.NativeEd25519 where

-- includes types
import Crypt.NaCl.Types
import Crypt.NaCl.Box (generateBoxKeyPair)
import Data.Function.Uncurried

foreign import verifyDetachedImpl :: Fn3 Signature Message SignPublicKey Boolean
foreign import signDetachedImpl :: Fn2 Message SignSecretKey Signature
foreign import genCurve25519KeyImpl :: Fn0 String

verifyDetached :: Signature -> Message -> SignPublicKey -> Boolean
verifyDetached = runFn3 verifyDetachedImpl

signDetached :: Message -> SignSecretKey -> Signature
signDetached = runFn2 signDetachedImpl

genCurve25519Key :: String
genCurve25519Key = runFn0 genCurve25519KeyImpl