module SecureVote.SPAs.SwarmMVP.Routes exposing (..)

import SecureVote.SPAs.SwarmMVP.DialogTypes exposing (DialogHtml)


type Route
    = SwmAddressR
    | SwmHowToVoteR
    | SwmVoteR
    | SwmSubmitR
    | NotFoundR


type DialogRoute msg
    = SettingsDialog
    | BallotDialog (DialogHtml msg)
    | InfoDialog
    | GethDialog
    | VerifyDialog
    | DebugDialog
    | NotFoundDialog
