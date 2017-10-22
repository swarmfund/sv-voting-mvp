module SecureVote.Utils.Time where
  

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Math (round)
  

currentTimestamp :: forall e. Eff (now :: NOW | e) Number
currentTimestamp = do
    nowInst <- now
    let (Milliseconds nowMs) = unInstant nowInst
    pure $ round $ nowMs / 1000.0