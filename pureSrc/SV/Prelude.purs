module SV.Prelude
    ( module Data.Maybe
    , module Data.Either
    , module Prelude
    , module Data.Tuple
    , module Control.Monad.Aff
    , module Control.Monad.Eff
    , eToAff
    , exceptToAff
    , mToAff
    ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff as AffM
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (Except, runExcept)
import Data.Either (Either(..), either, fromLeft, fromRight)
import Data.Maybe (Maybe(..), fromMaybe, maybe, fromJust, isJust, isNothing)
import Data.Tuple (Tuple(..), fst, snd)
import Global.Unsafe (unsafeStringify)


mToAff :: forall e a. String -> Maybe a -> Aff e a
mToAff err a = fromMaybe (throwError $ AffM.error err) (pure <$> a)


eToAff :: forall e eff a. Show e => Either e a -> Aff (| eff) a
eToAff e = case e of
    Right a -> pure a
    Left e -> throwError $ AffM.error $ show e


exceptToAff :: forall e eff a. Except e a -> Aff (| eff) a
exceptToAff e = either (\err -> throwError $ AffM.error $ "Unable to convert Except to Aff: " <> unsafeStringify err) pure $ runExcept e
