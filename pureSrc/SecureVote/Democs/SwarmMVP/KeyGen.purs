module SecureVote.Democs.SwarmMVP.KeyGen where
  


import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Crypt.NaCl (getBoxSecretKey, getBoxPublicKey, toUint8Array)
import Crypt.NaCl.Types (NACL_RANDOM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (defaultHelp, example, usage)
import SecureVote.Crypto.Curve25519 (genCurve25519Key)
import SecureVote.Utils.ArrayBuffer (toHex)


type KeyPair = { sk :: String, pk :: String }


generateKey :: forall e. Eff (naclRandom :: NACL_RANDOM | e) KeyPair
generateKey = do
    key <- genCurve25519Key
    let sk = toHex <<< toUint8Array $ getBoxSecretKey key
    let pk = toHex <<< toUint8Array $ getBoxPublicKey key
    pure {sk, pk}


generateAndShowKey :: forall eff. Eff (naclRandom :: NACL_RANDOM, console :: CONSOLE | eff) Unit
generateAndShowKey = do
            {sk, pk} <- generateKey
            log "############################################"
            log "## SWARM BALLOT ENCRYPTION KEY GENERATION ##"
            log "############################################"
            log ""
            log ""
            log "Here is your SECRET KEY. KEEP THIS PRIVATE:"
            log $ "sk: 0x" <> sk
            log ""
            log "Here is your PUBLIC KEY. PUBLISH THIS TO THE BLOCKCHAIN:"
            log $ "pk: 0x" <> pk
            log ""
            log ""
            log "The public key should be published before the ballot by providing it to the Eth smart contract constructor."
            log "The secret key should be kept secret until AFTER the ballot is closed. There is no recovery if you lose it.\n"
            
