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
    , goodInvestmentROI
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


ioTypes : BallotParams msg
ioTypes =
    { ballotTitle = "Which type of investment opportunities would you like to see on Swarm?"
    , contractAddr = "0x927304A1Cb2bb35b90C9b9BB35dcBaC2CB25c1Af"
    , description = "This non-binding ballot will help inform the Swarm Foundation of the type of funds to focus on in the near future."
    , endTime = 1618421966
    , startTime = 0
    , id = 3489673
    , erc20Balance = Nothing
    , erc20Abrv = swmErc20Abrv
    , erc20Addr = swmErc20Addr
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


goodInvestmentROI : BallotParams msg
goodInvestmentROI =
    { ballotTitle = "What do you consider a good investment? (Return vs Risk)"
    , contractAddr = "0x927304A1Cb2bb35b90C9b9BB35dcBaC2CB25c1Af"
    , description = "This non-binding ballot will help inform the Swarm Foundation of the type of funds to focus on in the near future."
    , endTime = 1618421966
    , startTime = 0
    , id = 3489349876
    , erc20Balance = Nothing
    , erc20Abrv = swmErc20Abrv
    , erc20Addr = swmErc20Addr
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
