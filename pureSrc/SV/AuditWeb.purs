module SV.AuditWeb where

import SV.Prelude

import Control.Monad.Aff (launchAff_, message)
import Control.Monad.Aff.Console as AffC
import Control.Monad.Error.Class (catchError)
import Data.Argonaut.Core as J
import Data.Int (toNumber)
import Data.StrMap as SMap
import Global.Unsafe (unsafeStringify)
import SV.Light.AuditApp (app, AppArgs)
import SV.Types.OutboundLogs (SUAux(..), OutAllDeets)
import SV.Utils.BigNumber (bnToStr)


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
-- | Balances Message `{t: "balances", p :: StrMap <toString Address> <toString Decimal>}`
-- | Delegations Message `{t: "delegations", p :: StrMap <toString Address> <toString Address>}`
updateF2 :: (_ -> Unit) -> {t :: String, p :: SUAux} -> Unit
updateF2 f ({t, p}) = case p of
    SuStr p_ -> f $ toJson J.fromString t p_
    SuRes p_ -> f $ toJson procRes t p_
    SuBal p_ -> f $ toJson J.fromString t p_
    SuDlgt p_ -> f $ toJson J.fromString t p_
  where
    toJson :: forall b. (b -> J.Json) -> String -> b -> J.Json
    toJson processP t p = J.fromObject $ SMap.insert "p" (processP p) $ SMap.singleton "t" $ J.fromString t
    procRes :: OutAllDeets -> J.Json
    procRes {nVotes, ballotResults} =
            J.fromObject
            $ SMap.insert "nVotes" (J.fromNumber $ toNumber nVotes)
            $ SMap.singleton "totals" $ J.fromObject $ (J.fromString <<< bnToStr) <$> ballotResults
