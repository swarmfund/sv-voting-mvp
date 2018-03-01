module SecureVote.Ballots.LittleGovIndexes exposing (..)

import BigInt as BI
import Web3.Eth.Contract as Contract
import Web3.Types exposing (Abi(Abi), Address(Address), Sha3(Sha3))


type alias NetworkTo a =
    { kovan : a, mainnet : a, ropsten : a }



{-
   NOTE: THIS IS NOT WHERE ADDRESSES ARE PULLED FROM
   They are environment variables!
-}
--
--getIndex dev =
--    if dev then
--        littleGovIndexs.kovan
--    else
--        littleGovIndexs.mainnet
--
--
--littleGovIndexs : NetworkTo String
--littleGovIndexs =
--    { mainnet = ""
--    , kovan = "0xb047E50088b7f2f5B4860a5bf0941E035F2BeaA3"
--    , ropsten = ""
--    }
--
--
--
---- Old kovan addrs:
---- "0x8D78A6d6f9d6a8785c28eb317c7E01A02D61C5ca"
---- "0xB7C27F17cb255D89E6aFF91c483dC2F5647637f3"
--
--
--defaultDemocs : NetworkTo String
--defaultDemocs =
--    { mainnet = ""
--    , kovan = "0x89a31f317ec72391152253bda9673a1047cca994f52e33f50112bec3f462d9bd"
--    , ropsten = ""
--    }
