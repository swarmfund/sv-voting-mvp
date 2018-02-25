module SecureVote.SPAs.SwarmMVP.Ballots.ReleaseSchedule exposing (..)

import List exposing (map)
import List.Extra exposing (zip)
import Round as R
import SecureVote.SPAs.SwarmMVP.Ballots.Types exposing (BallotOption, BallotParams, ReleaseSchedule)
import SecureVote.SPAs.SwarmMVP.Const exposing (swmErc20Abrv, swmErc20Addr)
import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml(DlogDiv, DlogLi, DlogP, DlogTxt, DlogUl))


rschedBallot : BallotParams msg
rschedBallot =
    { voteOptions = voteOptionsRSched
    , openingDesc = rSchedOpeningDesc
    , contractAddr = "0x2Bb10945E9f0C9483022dc473aB4951BC2a77d0f"
    , startTime = 1509372000
    , endTime = 1510009200
    , ballotTitle = "SWM Token Release Schedule"
    , description = "This ballot will decide the release schedule of SWM tokens. There are four possible schedules, differring in the size and frequency of each step in the schedule."
    , erc20Addr = swmErc20Addr
    , erc20Balance = Nothing
    , erc20Abrv = swmErc20Abrv
    , id = 3948759378
    , discussionLink = Nothing
    }


rSchedOpeningDesc : String
rSchedOpeningDesc =
    "This ballot is to decide the release schedule of SWM tokens."


renderReleaseScheduleTitle : ReleaseSchedule -> String
renderReleaseScheduleTitle { nReleases, releaseLength } =
    let
        sOptional =
            if nReleases > 1 then
                "s"
            else
                ""
    in
    toString nReleases ++ " release" ++ sOptional ++ " of " ++ toString releaseLength ++ " days"


rSchedToListElems : ReleaseSchedule -> List (DialogHtml msg)
rSchedToListElems rSched =
    let
        txtToLi =
            DlogLi << DlogTxt

        nRel =
            rSched.nReleases

        relLen =
            rSched.releaseLength

        nRelS =
            if nRel > 1 then
                "s"
            else
                ""

        relLenS =
            if relLen > 1 then
                "s"
            else
                ""

        relLenStr =
            toString relLen

        nRelStr =
            toString nRel
    in
    [ txtToLi <|
        "This release involves "
            ++ nRelStr
            ++ " release"
            ++ nRelS
            ++ " over "
            ++ relLenStr
            ++ " day"
            ++ relLenS
            ++ "."
    , txtToLi <|
        "At the conclusion of each "
            ++ relLenStr
            ++ " day period, approximately "
            ++ (R.round 2 <| 100.0 / toFloat nRel)
            ++ "% of tokens will be released, proportionally across all token holders."
    , txtToLi <|
        "This means the full liquidity release schedule will take "
            ++ (toString <| (nRel * relLen))
            ++ " days to complete."
    ]


addBallotDescRSched : List (DialogHtml msg) -> DialogHtml msg -> DialogHtml msg
addBallotDescRSched dialogElems desc =
    DlogDiv
        [ DlogP [ DlogTxt rSchedOpeningDesc ]
        , DlogUl dialogElems
        , desc
        ]


voteOptionsRSched : List (BallotOption msg)
voteOptionsRSched =
    let
        wrapP inner =
            DlogP [ DlogTxt inner ]

        modRSched ( r, f ) =
            f r
    in
    map modRSched <|
        zip rScheds
            [ \a -> BallotOption 1337000001 (renderReleaseScheduleTitle a) <| addBallotDescRSched (rSchedToListElems a) (wrapP "This is the proposal in the whitepaper. The release will take approximately 1 year.")
            , \a -> BallotOption 1337000002 (renderReleaseScheduleTitle a) <| addBallotDescRSched (rSchedToListElems a) (wrapP "This is like the release schedule in the whitepaper, but smaller chunks will be released more frequently.")
            , \a -> BallotOption 1337000003 (renderReleaseScheduleTitle a) <| addBallotDescRSched (rSchedToListElems a) (wrapP "This release will occur over 2 years.")
            , \a -> BallotOption 1337000004 (renderReleaseScheduleTitle a) <| addBallotDescRSched (rSchedToListElems a) (wrapP "This is similar to option 1 (8 releases of 42 days), but less frequent.")
            ]


rScheds : List ReleaseSchedule
rScheds =
    [ ReleaseSchedule 8 42, ReleaseSchedule 42 8, ReleaseSchedule 16 42, ReleaseSchedule 4 84 ]


doBallotOptsMatchRSched : List (List Int) -> Bool
doBallotOptsMatchRSched optsFromEth =
    let
        extractRelSched rSchedule =
            let
                { nReleases, releaseLength } =
                    rSchedule
            in
            [ nReleases, releaseLength ]

        releaseSchedules =
            List.map extractRelSched rScheds

        matches =
            List.map2 (\ethOpt ourOpt -> ethOpt == ourOpt) optsFromEth releaseSchedules
    in
    List.all (\b -> b) matches
