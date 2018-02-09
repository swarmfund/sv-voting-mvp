module SecureVote.Democs.SwarmMVP.AuditWeb where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Crypt.NaCl (NACL_RANDOM)
import SecureVote.Democs.SwarmMVP.AuditApp (app)


main :: forall e. String -> String -> String -> String -> Eff ( console :: CONSOLE, naclRandom :: NACL_RANDOM, now :: NOW | e) Unit
main ethUrl ethAuth votingAddr erc20Addr = launchAff_ $ app ethUrl ethAuth votingAddr erc20Addr
