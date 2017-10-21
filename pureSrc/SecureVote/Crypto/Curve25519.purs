module SecureVote.Crypto.Curve25519 where
  
import Crypt.NaCl.Box (generateBoxKeyPair)


genCurve25519Key = generateBoxKeyPair