module SecureVote.SPAs.SwarmMVP.Routes exposing (..)

import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml)


type Route
    = SwmAddressR
    | OpeningSlideR
    | SwmHowToVoteR
    | SwmVoteR
    | SwmDelegateR
    | SwmSubmitR
    | ListAllVotesR
    | NotFoundR


defaultRoute : Route
defaultRoute =
    ListAllVotesR


type DialogRoute msg
    = SettingsDialog
    | BallotDialog (DialogHtml msg)
    | InfoDialog
    | GethDialog
    | MEWDialog
    | VerifyDialog
    | FullAuditDialog
    | NotFoundDialog
