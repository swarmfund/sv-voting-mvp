module SV.AuditWeb where

import SV.Prelude

import Control.Monad.Aff (launchAff_, liftEff', message)
import Control.Monad.Aff.Console as AffC
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
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
import Global.Unsafe (unsafeStringify)
import SV.Light.AuditApp (app, AppArgs)
import SV.Types.OutboundLogs (SUAux(..))


data DOut a = DStr String | DOther a


main :: forall a e eff. AppArgs -> (J.Json -> Unit) -> Eff _ Unit
main args updateF = launchAff_ $ do
    let updateF_ = updateF2 updateF
    let failErrorCode = 1
    AffC.log $ "AuditWeb starting with args: " <> unsafeStringify args
    _ <- catchError (app args updateF_) \e -> do
                let _ = updateF_ {t: "fail", p: SuStr $ message e}
                pure $ Left $ Tuple failErrorCode $ message e
    pure unit


-- | Transform StatusUpdate values into JSON values with one of the structures: (for inter-compatibility with elm)
-- | Log Message: `{t: "log", p: <msg :: String>}`
-- | Fail Message: `{t: "fail", p: <errMsg :: String>}`
-- | Success Message `{t: "success", p :: {nVotes :: Number, totals :: { <optionName :: String>: <totalAsEncodedDecimal :: String> }}}`
-- | Balances Message `{t : "balances", p :: Array (Tuple <toString Address> <toString Decimal>)}`
-- | Delegations Message `{t : "delegations", p :: Array (Tuple <toString Address> <toString Address>)}`
updateF2 :: forall a. (_ -> Unit) -> {t :: String, p :: SUAux} -> Unit
updateF2 f ({t, p}) = case p of
    SuStr p_ -> f $ toJson J.fromString t p_
    SuRes p_ -> f $ toJson procRes t p_
    SuBal p_ -> f $ toJson J.fromString t p_
    SuDlgt p_ -> f $ toJson J.fromString t p_
  where
    toJson :: forall b. (b -> J.Json) -> String -> b -> J.Json
    toJson processP t p = J.fromObject $ SMap.insert "p" (processP p) $ SMap.singleton "t" $ J.fromString t
    procRes {nVotes, ballotResults: {yes, no}} =
            J.fromObject
            $ SMap.insert "nVotes" (J.fromNumber $ toNumber nVotes)
            $ SMap.singleton "totals" $ J.fromObject $ SMap.fromFoldable $ (map (J.fromString)) <$> [Tuple "yes" yes, Tuple "no" no]
