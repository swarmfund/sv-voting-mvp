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


type DialogRoute msg
    = SettingsDialog
    | BallotDialog (DialogHtml msg)
    | InfoDialog
    | GethDialog
    | MEWDialog
    | VerifyDialog
    | NotFoundDialog
