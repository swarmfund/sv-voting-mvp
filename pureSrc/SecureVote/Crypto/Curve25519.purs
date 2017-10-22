module SecureVote.Crypto.Curve25519 where
  
import Crypt.NaCl.Box (generateBoxKeyPair, box)


genCurve25519Key = generateBoxKeyPair


-- encrypt25519 