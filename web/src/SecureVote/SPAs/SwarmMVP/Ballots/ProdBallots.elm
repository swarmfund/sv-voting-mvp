module SecureVote.SPAs.SwarmMVP.Ballots.ProdBallots exposing (..)

import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotOption, BallotParams, ChainIndex(..))
import SecureVote.SPAs.SwarmMVP.Const exposing (swmErc20Addr)
import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml(DlogTxt))


getChainIndexFor i =
    case i of
        1002000 ->
            5173410

        1001000 ->
            5173410

        _ ->
            -1


renderedGetCIFor i =
    let
        ci =
            getChainIndexFor i
    in
    if ci < 0 then
        "the most recent block"
    else
        "block " ++ toString ci


marchSurveyROI : BallotParams msg
marchSurveyROI =
    { voteOptions = roiOptions
    , ballotTitle = "Asset-backed token investments are attractive when their minimum annual return is:"
    , contractAddr = "0x6B649662dA40F10361F008b481143029296a69D6"
    , description = "This non-binding ballot will help the Swarm Foundation gauge the risk profile of the SWM token holders."
    , openingDesc = "This non-binding ballot will help the Swarm Foundation gauge the risk profile of the SWM token holders."
    , endTime = 1522540740
    , startTime = 1519851600
    , id = 1002000
    , erc20Balance = Nothing
    , erc20Abrv = "SWM"
    , erc20Addr = swmErc20Addr
    , discussionLink = Just "https://discuss.swarm.fund/t/asset-backed-token-investments-are-attractive-when-their-minimum-annual-return-is/46/1"
    }


roiOptions : List (BallotOption msg)
roiOptions =
    [ { id = 1002001, title = "5% - No risk", description = DlogTxt "" }
    , { id = 1002002, title = "25% - Some risk", description = DlogTxt "" }
    , { id = 1002003, title = "100%+ - High risk", description = DlogTxt "" }
    , { id = 1002004, title = "10X - VC territory", description = DlogTxt "" }
    , { id = 1002005, title = "100X - “Insane, do you call this an investment?”", description = DlogTxt "" }
    ]


marchSurveyIOTypes : BallotParams msg
marchSurveyIOTypes =
    { voteOptions = ioTypesOptions
    , ballotTitle = "Which type of investment opportunities would you like to see on Swarm?"
    , contractAddr = "0x1e6b7d459AF96E916548D27B0e72ce17ccb7dB74"
    , description = "This non-binding ballot will help inform the Swarm Foundation of the type of funds to focus on in the near future."
    , openingDesc = "This non-binding ballot will help inform the Swarm Foundation of the type of funds to focus on in the near future."
    , endTime = 1522540740
    , startTime = 1519851600
    , id = 1001000
    , erc20Balance = Nothing
    , erc20Abrv = "SWM"
    , erc20Addr = swmErc20Addr
    , discussionLink = Just "https://discuss.swarm.fund/t/which-type-of-investment-opportunities-would-you-like-to-see-on-swarm/45/2"
    }


ioTypesOptions : List (BallotOption msg)
ioTypesOptions =
    [ { id = 1001001, title = "Real Estate", description = DlogTxt "" }
    , { id = 1001002, title = "Crypto hedge funds", description = DlogTxt "" }
    , { id = 1001003, title = "Renewables Funds (solar, geothermal, etc)", description = DlogTxt "" }
    , { id = 1001004, title = "Impact Funds", description = DlogTxt "" }
    , { id = 1001005, title = "Cannabis Funds", description = DlogTxt "" }
    ]
