module SecureVote.SPAs.SwarmMVP.Fields exposing (..)


lsAddrId =
    "voting.eth.address"


lsBallotsVotedId =
    "voting.ballotsVotedOn"


lsPendingVotesId =
    "voting.votesPending"


userErc20AddrId : String
userErc20AddrId =
    "ethAddress"


userTempErc20AddrId : String
userTempErc20AddrId =
    "eth.address.temp"


txidCheckId : String
txidCheckId =
    "ballotTxid"


defaultDelegate : String
defaultDelegate =
    "0x0000000000000000000000000000000000000000"


ethNodeTemp : String
ethNodeTemp =
    "ethNodeUrl"



-- This describes the number of options a user can select, from -1*ballotRangeAbs to ballotRangeAbs


ballotRangeSize : Int
ballotRangeSize =
    ballotRangeAbs * 2 + 1


ballotMax : Int
ballotMax =
    ballotRangeAbs * 2


ballotRangeAbs : Int
ballotRangeAbs =
    3


ballotDisplayMax : Int
ballotDisplayMax =
    0 + ballotRangeAbs


ballotDisplayMin : Int
ballotDisplayMin =
    0 - ballotRangeAbs
