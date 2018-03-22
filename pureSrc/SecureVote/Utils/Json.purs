module SecureVote.Utils.Json where

import SV.Prelude

import Control.Monad.Error.Class (throwError)
import Data.Foreign (F, ForeignError(..))
import Data.List.NonEmpty as NEL


mkFErr :: forall a. String -> F a
mkFErr = throwError <<< NEL.singleton <<< ForeignError
