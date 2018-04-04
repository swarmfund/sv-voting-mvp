module SV.Types.OutboundLogs where

import SV.Prelude

import Data.StrMap (StrMap)
import Network.Ethereum.Web3 (BigNumber)


type OutAllDeets = {nVotes :: Int, ballotResults :: StrMap BigNumber}

data SUAux = SuStr String | SuRes OutAllDeets | SuBal String | SuDlgt String

type StatusUpdate = {t :: String, p :: SUAux}


mkSUFail :: String -> StatusUpdate
mkSUFail e = {t: "fail", p: SuStr e}


mkSULog :: String -> StatusUpdate
mkSULog p = {t: "log", p: SuStr p}


mkSUSuccess :: OutAllDeets -> StatusUpdate
mkSUSuccess b = {t: "success", p: SuRes b}

mkSUWarn :: String -> StatusUpdate
mkSUWarn m = {t: "warn", p: SuStr m}
