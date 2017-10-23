module SecureVote.Crypto.NativeEd25519 where

-- includes types
import Crypt.NaCl.Types
import Data.Function.Uncurried

import Crypt.NaCl.Box (generateBoxKeyPair)
import Data.ArrayBuffer.Types (Uint8Array)

foreign import verifyDetachedImpl :: Fn3 Signature Message SignPublicKey Boolean
foreign import signDetachedImpl :: Fn2 Message SignSecretKey Signature
foreign import genCurve25519KeyImpl :: Fn0 String
foreign import sha256Impl :: Fn1 Uint8Array Uint8Array

verifyDetached :: Signature -> Message -> SignPublicKey -> Boolean
verifyDetached = runFn3 verifyDetachedImpl

signDetached :: Message -> SignSecretKey -> Signature
signDetached = runFn2 signDetachedImpl

genCurve25519Key :: String
genCurve25519Key = runFn0 genCurve25519KeyImpl

sha256 :: Uint8Array -> Uint8Array
sha256 = runFn1 sha256Impl