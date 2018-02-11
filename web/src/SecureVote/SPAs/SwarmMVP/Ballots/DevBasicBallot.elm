module SecureVote.SPAs.SwarmMVP.Ballots.DevBasicBallot exposing (..)

import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotOption, BallotParams)
import SecureVote.SPAs.SwarmMVP.Const exposing (devErc20Addr)


devBasicBallot : BallotParams msg
devBasicBallot =
    { voteOptions = devBBVoteOpts
    , ballotTitle = "Dev Test Basic Ballot"
    , contractAddr = "0x4e5f2729d18692098dc886a32e73fa305f63647d"
    , description = "A basic ballot - this is the description"
    , endTime = 1518334329
    , startTime = 1518324329
    , id = 38495739457
    , erc20Balance = Nothing
    , erc20Abrv = "DEVERC20"
    , erc20Addr = devErc20Addr
    , openingDesc = "This is the opening description for the basic ballot!!!"
    }


devBBVoteOpts : List (BallotOption msg)
devBBVoteOpts =
    []
