module SecureVote.SPAs.SwarmMVP.Ballot exposing (..)

import Round as R
import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml(..))


openingDesc : String
openingDesc =
    "This option for the release of SWM tokens corresponds to the following specification:"


type alias BallotOption msg =
    { id : Int
    , rSchedule : ReleaseSchedule
    , description : DialogHtml msg
    }


type alias ReleaseSchedule =
    { nReleases : Int
    , releaseLength : Int
    }


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


addBallotDesc : List (BallotOption msg) -> List (BallotOption msg)
addBallotDesc ballotOptions =
    let
        enhanceDesc rSched desc =
            DlogDiv
                [ DlogP [ DlogTxt openingDesc ]
                , DlogUl <| rSchedToListElems rSched
                , desc
                ]

        expandDesc { id, rSchedule, description } =
            BallotOption id rSchedule <| enhanceDesc rSchedule description
    in
    List.map expandDesc ballotOptions


voteOptions : List (BallotOption msg)
voteOptions =
    let
        wrapP inner =
            DlogP [ DlogTxt inner ]
    in
    addBallotDesc <|
        [ BallotOption 1337000001 (ReleaseSchedule 8 42) <| wrapP "This is the proposal in the whitepaper. The release will take approximately 1 year."
        , BallotOption 1337000002 (ReleaseSchedule 42 8) <| wrapP "This is like the release schedule in the whitepaper, but smaller chunks will be released more frequently."
        , BallotOption 1337000003 (ReleaseSchedule 16 42) <| wrapP "This release will occur over 2 years."
        , BallotOption 1337000004 (ReleaseSchedule 4 84) <| DlogP [ DlogTxt "This is similar to option 1 (8 releases of 42 days), but less frequent." ]
        ]


doBallotOptsMatch : List (List Int) -> Bool
doBallotOptsMatch optsFromEth =
    let
        extractRelSched { rSchedule } =
            let
                { nReleases, releaseLength } =
                    rSchedule
            in
            [ nReleases, releaseLength ]

        releaseSchedules =
            List.map extractRelSched voteOptions

        matches =
            List.map2 (\ethOpt ourOpt -> ethOpt == ourOpt) optsFromEth releaseSchedules
    in
    List.all (\b -> b) matches
