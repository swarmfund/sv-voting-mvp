module SecureVote.Crypto.NativeEd25519 where

-- includes types
import Data.Function.Uncurried

import Data.ArrayBuffer.Types (Uint8Array)

foreign import sha256Impl :: Fn1 Uint8Array Uint8Array

sha256 :: Uint8Array -> Uint8Array
sha256 = runFn1 sha256Impl
