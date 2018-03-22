module SV.Types.OutboundLogs where

import SV.Prelude

import Data.Decimal (Decimal)
import Network.Ethereum.Web3 (BigNumber)
import SV.Light.Types.RunBallot (BallotResult)


type OutAllDeets = {nVotes :: Int, ballotResults :: {yes :: String, no :: String}}

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
