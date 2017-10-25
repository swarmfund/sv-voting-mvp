module SecureVote.Democs.SwarmMVP.Admin where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Crypt.NaCl (getBoxSecretKey, toUint8Array)
import Crypt.NaCl.Types (NACL_RANDOM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (defaultHelp, demandCount, example, usage)
import SecureVote.Crypto.Curve25519 (genCurve25519Key)
import SecureVote.Democs.SwarmMVP.KeyGen (generateAndShowKey)


app :: forall eff. String -> Boolean -> Eff (console :: CONSOLE, naclRandom :: NACL_RANDOM | eff) Unit
app ethUrl fGenBallotKey = 
        if fGenBallotKey then 
          do generateAndShowKey
        else
          do 
            log "No valid commands given."
            log "Check --help"


main :: forall eff. Eff (console :: CONSOLE, exception :: EXCEPTION, naclRandom :: NACL_RANDOM | eff) Unit
main = do
  let setup = usage "$0 [--genBallotKey]"
              -- <> example "$0 -w Hello -w World" "Say hello!"
              <> defaultHelp

  runY setup $ app <$> yarg "e" ["ethnode"] (Just "HTTP URL of Eth node") (Left "https://mainnet.infura.io") true
                   <*> flag "genBallotKey" [] (Just "Generate the ballot encryption key")


