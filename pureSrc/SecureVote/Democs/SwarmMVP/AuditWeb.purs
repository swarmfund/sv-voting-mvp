module SecureVote.Democs.SwarmMVP.AuditWeb where

import Prelude

import Control.Monad.Aff (launchAff_, liftEff', message)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Error.Class (catchError)
import Crypt.NaCl (NACL_RANDOM)
import Data.Argonaut.Core as J
import Data.Decimal as D
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.StrMap as SMap
import Data.Tuple (Tuple(..))
import SecureVote.Democs.SwarmMVP.AuditApp (app, AppArgs)
import SecureVote.Democs.SwarmMVP.BallotContract (StatusUpdate(..), SUAux(..))


data DOut a = DStr String | DOther a


main :: forall a e eff. AppArgs -> (J.Json -> Unit) -> Eff (console :: CONSOLE, naclRandom :: NACL_RANDOM, now :: NOW, exception :: EXCEPTION | e) Unit
main args updateF = launchAff_ $ do
    let updateF_ = updateF2 updateF
    let failErrorCode = 1
    _ <- catchError (app args updateF_) \e -> do
                let _ = updateF_ {t: "fail", p: SuStr $ show e}
                pure $ Left $ Tuple failErrorCode $ message e

    pure unit


-- | Transform StatusUpdate values into JSON values with one of the structures:
-- | Log Message: `{t: "log", p: <msg :: String>}`
-- | Fail Message: `{t: "fail", p: <errMsg :: String>}`
-- | Success Message `{t: "success", p :: {nVotes :: Number, totals :: { <optionName :: String>: <totalAsEncodedDecimal :: String> }}}`
updateF2 :: forall a. (_ -> Unit) -> {t :: String, p :: SUAux} -> Unit
updateF2 f ({t, p}) = case p of
    SuStr p_ -> f $ toJson J.fromString t p_
    SuRes p_ -> f $ toJson procRes t p_
  where
    toJson :: forall b. (b -> J.Json) -> String -> b -> J.Json
    toJson processP t p = J.fromObject $ SMap.insert "p" (processP p) $ SMap.singleton "t" $ J.fromString t
    procRes {nVotes, ballotResult: {totals}} =
            J.fromObject
            $ SMap.insert "nVotes" (J.fromNumber $ toNumber nVotes)
            $ SMap.singleton "totals" $ J.fromObject $ SMap.fromFoldable $ (map (J.fromString <<< D.toString)) <$> totals
