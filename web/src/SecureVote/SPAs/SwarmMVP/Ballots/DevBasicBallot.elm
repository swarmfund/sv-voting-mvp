module SecureVote.SPAs.SwarmMVP.Ballots.DevBasicBallot exposing (..)

import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotOption, BallotParams)
import SecureVote.SPAs.SwarmMVP.Const exposing (devErc20Addr, swmErc20Abrv, swmErc20Addr)
import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml(..))


allDevBallots_ : List (BallotParams msg)
allDevBallots_ =
    [ devBasicBallot
    , devBasic2
    , devBasic3
    , devBasic4
    , devBasic5
    , devBasic6
    , devBasic7
    , ioTypes
    , marchT1Ballot
    ]


marchT1Ballot : BallotParams msg
marchT1Ballot =
    { voteOptions = marchT1Opts
    , ballotTitle = "Asset-backed token investments are attractive when their minimum annual return is:"
    , contractAddr = "0x76a4E883944906409432464AB1EdDc90dd2543cA"
    , description = "This non-binding ballot will help the Swarm Foundation gauge the risk profile of the SWM token holders."
    , openingDesc = "This non-binding ballot will help the Swarm Foundation gauge the risk profile of the SWM token holders."
    , endTime = 1522540740
    , startTime = 1519851600
    , id = 83947539478
    , erc20Balance = Nothing
    , erc20Abrv = "SWM"
    , erc20Addr = devErc20Addr
    , discussionLink = Just "https://discuss.swarm.fund/t/asset-backed-token-investments-are-attractive-when-their-minimum-annual-return-is/46/1"
    }


marchT1Opts : List (BallotOption msg)
marchT1Opts =
    [ { id = 4634534, title = "5% - No risk", description = DlogTxt "" }
    , { id = 3462542, title = "25% - Some risk", description = DlogTxt "" }
    , { id = 3456456, title = "100%+ - High risk", description = DlogTxt "" }
    , { id = 4567456, title = "10X - VC territory", description = DlogTxt "" }
    , { id = 3464567, title = "100X - “Insane, do you call this an investment?”", description = DlogTxt "" }
    ]


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
    , discussionLink = Just "https://secure.vote"
    }


devBBVoteOpts : List (BallotOption msg)
devBBVoteOpts =
    []


devBasic2 : BallotParams msg
devBasic2 =
    { ballotTitle = "Dev test 2"
    , contractAddr = "0x2cdb6b361ecc7a834ce8a3a78556e70c3e74660e"
    , description = "Second test ballot"
    , endTime = 1518389860
    , startTime = 1518388860
    , id = 9238467593
    , erc20Balance = Nothing
    , erc20Abrv = "LEET"
    , erc20Addr = devErc20Addr
    , openingDesc = "This is the description of the second test ballot"
    , voteOptions = devBasic2VOs
    , discussionLink = Just "https://secure.vote"
    }


devBasic2VOs : List (BallotOption msg)
devBasic2VOs =
    [ { id = 394587392, title = "Option 1", description = DlogTxt "desc opt 1" }
    , { id = 494647867, title = "Option 2", description = DlogTxt "desc opt 2" }
    , { id = 324536984, title = "Option 3 with some caveat and a long title", description = DlogTxt "desc opt 3" }
    ]


devBasic3 : BallotParams msg
devBasic3 =
    { ballotTitle = "Dev test 3"
    , contractAddr = "0x4540D2EF5E8048Db8B16eB857A7AbC4ba69ccC57"
    , description = "Third test ballot"
    , endTime = 1518490261
    , startTime = 1518390261
    , id = 92384239487
    , erc20Balance = Nothing
    , erc20Abrv = "LEET"
    , erc20Addr = devErc20Addr
    , openingDesc = "This is the description of the third test ballot"
    , voteOptions =
        [ { id = 3943587392, title = "Option 1", description = DlogTxt "desc opt 1" }
        , { id = 4946547867, title = "Option 2", description = DlogTxt "desc opt 2" }
        , { id = 3234536984, title = "Option 3 with some caveat and a long title", description = DlogTxt "desc opt 3" }
        ]
    , discussionLink = Just "https://secure.vote"
    }


devBasic4 : BallotParams msg
devBasic4 =
    { ballotTitle = "Dev test 4"
    , contractAddr = "0xAedD71A3c878A4B3B241Ef036f28428cdFf8DBd5"
    , description = "Fourth test ballot"
    , endTime = 1519999999
    , startTime = 1519390261
    , id = 9233498487
    , erc20Balance = Nothing
    , erc20Abrv = "LEET"
    , erc20Addr = devErc20Addr
    , openingDesc = "This is the description of the fourth test ballot"
    , voteOptions =
        [ { id = 3943587392, title = "Option 1", description = DlogTxt "desc opt 1" }
        , { id = 4946547867, title = "Option 2", description = DlogTxt "desc opt 2" }
        , { id = 3234536984, title = "Option 3 with some caveat and a long title", description = DlogTxt "desc opt 3" }
        ]
    , discussionLink = Just "https://secure.vote"
    }


devBasic5 : BallotParams msg
devBasic5 =
    { ballotTitle = "Dev test 5"
    , contractAddr = "0xCE877C405180f55E0e9730226f2B3b8A277f1611"
    , description = "5th test ballot for testing. It decides nothing. This is the long description."
    , endTime = 0
    , startTime = 1518400000
    , id = 9234392329
    , erc20Balance = Nothing
    , erc20Abrv = "DEV"
    , erc20Addr = devErc20Addr
    , openingDesc = "This ballot is the 5th test ballot to decide nothing. Short description."
    , voteOptions =
        [ { id = 3943587392, title = "Option 1", description = DlogTxt "desc opt 1" }
        , { id = 4946547867, title = "Option 2", description = DlogTxt "desc opt 2" }
        , { id = 3234536984, title = "Option 3 with some caveat and a long title", description = DlogTxt "desc opt 3" }
        ]
    , discussionLink = Just "https://secure.vote"
    }


devBasic6 : BallotParams msg
devBasic6 =
    { ballotTitle = "Dev test 6"
    , contractAddr = "0x61dd63C8EB3344903461B8eE35BF8ad5b8f507a3"
    , description = "6th test ballot for testing. It decides nothing. This is the long description."
    , endTime = 1518421966
    , startTime = 1518420000
    , id = 92343490
    , erc20Balance = Nothing
    , erc20Abrv = "DEV"
    , erc20Addr = devErc20Addr
    , openingDesc = "This ballot is the 6th test ballot to decide nothing. Short description."
    , voteOptions =
        [ { id = 3943587392, title = "Option 1", description = DlogTxt "desc opt 1" }
        , { id = 4946547867, title = "Option 2", description = DlogTxt "desc opt 2" }
        , { id = 3234536984, title = "Option 3 with some caveat and a long title", description = DlogTxt "desc opt 3" }
        , { id = 4946547868, title = "the last option", description = DlogTxt "desc opt 4" }
        , { id = 4946547869, title = "not really", description = DlogTxt "desc opt 5" }
        ]
    , discussionLink = Just "https://secure.vote"
    }


devBasic7 : BallotParams msg
devBasic7 =
    { ballotTitle = "Dev Perpetual Ballot"
    , contractAddr = "0x927304A1Cb2bb35b90C9b9BB35dcBaC2CB25c1Af"
    , description = "Perpetual ballot for testing, note that option 1 and 2 should have disabled buttons for description."
    , endTime = 2018421966
    , startTime = 0
    , id = 3904856
    , erc20Balance = Nothing
    , erc20Abrv = "DEV"
    , erc20Addr = devErc20Addr
    , openingDesc = "This is a perpetual ballot for testing."
    , voteOptions =
        [ { id = 3943587392, title = "Option 1", description = DlogTxt "" }
        , { id = 4946547867, title = "Option 2", description = DlogTxt "" }
        , { id = 3234536984, title = "Option 3 with some caveat and a long title", description = DlogTxt "desc opt 3" }
        , { id = 4946547868, title = "the last option", description = DlogTxt "desc opt 4" }
        , { id = 4946547869, title = "not really", description = DlogTxt "desc opt 5" }
        ]
    , discussionLink = Just "https://secure.vote"
    }



{-
   deploy cmd

   node bin/solidity/deploy.js --testing --startTime 1519516800 --endTime 1520121600 --ballotEncPubkey 0x07b86b2ed34fe9c2f69b0840a41a1d09bbdb4ec52565b909f32d8491a9dbdd7b --optionNamesJson '["Real Estate", "Crypto hedge funds", "Renewables Funds (solar, geo thermal, etc)", "Impact Funds", "Cannabis Funds"]'
-}


ioTypes : BallotParams msg
ioTypes =
    { ballotTitle = "Which type of investment opportunities would you like to see on Swarm?"
    , contractAddr = "0x080dba74c58775afce8ed4d0306bb44322f1a134"
    , description = "This non-binding ballot will help inform the Swarm Foundation of the type of funds to focus on in the near future."
    , endTime = 1520121600
    , startTime = 1519516800
    , id = 3489673
    , erc20Balance = Nothing
    , erc20Abrv = swmErc20Abrv

    -- , erc20Addr = swmErc20Addr
    , erc20Addr = devErc20Addr
    , openingDesc = "This non-binding ballot will help inform the Swarm Foundation of the type of funds to focus on in the near future."
    , discussionLink = Just "https://discuss.swarm.fund/t/which-type-of-investment-opportunities-would-you-like-to-see-on-swarm/45/1"
    , voteOptions =
        [ { id = 8349834983, title = "Real Estate", description = DlogTxt "" }
        , { id = 8349834984, title = "Crypto hedge funds", description = DlogTxt "" }
        , { id = 8349834985, title = "Renewables Funds (solar, geo thermal, etc)", description = DlogTxt "" }
        , { id = 8349834986, title = "Impact Funds", description = DlogTxt "" }
        , { id = 8349834987, title = "Cannabis Funds", description = DlogTxt "" }
        ]
    }



{-
   deploy cmd

   node bin/solidity/deploy.js --testing --startTime 1519516800 --endTime 1520121600 --ballotEncPubkey 0x07b86b2ed34fe9c2f69b0840a41a1d09bbdb4ec52565b909f32d8491a9dbdd7b --optionNamesJson '["5% - No risk", "25% - Some risk", "100%+ - High risk", "10X - VC territory", "100X - “Insane, do you call this an investment?”"]'
-}


goodInvestmentROI : BallotParams msg
goodInvestmentROI =
    { ballotTitle = "Asset-backed token investments are attractive when their minimum annual return is:"
    , contractAddr = "0x5d66847071924595fe9e1020a19323e31afc9276"
    , description = "This non-binding ballot will help inform the Swarm Foundation of the type of funds to focus on in the near future."
    , endTime = 1520121600
    , startTime = 1519516800
    , id = 3489349876
    , erc20Balance = Nothing
    , erc20Abrv = swmErc20Abrv

    -- , erc20Addr = swmErc20Addr
    , erc20Addr = devErc20Addr
    , openingDesc = "This non-binding ballot will help inform the Swarm Foundation of the type of funds to focus on in the near future."
    , discussionLink = Just "https://discuss.swarm.fund/t/which-type-of-investment-opportunities-would-you-like-to-see-on-swarm/45/1"
    , voteOptions =
        [ { id = 8449834983, title = "5% - No risk", description = DlogTxt "" }
        , { id = 8449834984, title = "25% - Some risk", description = DlogTxt "" }
        , { id = 8449834985, title = "100%+ - High risk", description = DlogTxt "" }
        , { id = 8449834986, title = "10X - VC territory", description = DlogTxt "" }
        , { id = 8449834987, title = "100X - “Insane, do you call this an investment?”", description = DlogTxt "" }
        ]
    }
