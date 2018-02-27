module SecureVote.Ballots.LittleGovIndexes exposing (..)

import BigInt as BI
import Web3.Eth.Contract as Contract
import Web3.Types exposing (Abi(Abi), Address(Address), Sha3(Sha3))


type alias NetworkTo a =
    { kovan : a, mainnet : a, ropsten : a }


getIndex dev =
    if dev then
        littleGovIndexs.kovan
    else
        littleGovIndexs.mainnet


littleGovIndexs : NetworkTo String
littleGovIndexs =
    { mainnet = ""
    , kovan = "0x53E4E4c2EAC4Ac300963FB711Eb763AF395071cA"
    , ropsten = ""
    }



-- Old kovan addrs:
-- "0x8D78A6d6f9d6a8785c28eb317c7E01A02D61C5ca"
-- "0xB7C27F17cb255D89E6aFF91c483dC2F5647637f3"


defaultDemocs : NetworkTo String
defaultDemocs =
    { mainnet = ""
    , kovan = "0x2b190610388e1a501323333f525ea3e6d896164352a7219c175f4e8d6000885d"
    , ropsten = ""
    }
