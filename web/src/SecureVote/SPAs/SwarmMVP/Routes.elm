module SecureVote.SPAs.SwarmMVP.Routes exposing (..)


type Route
    = SwmAddressR
    | SwmVoteR
    | SwmSubmitR
    | NotFoundR


type DialogRoute
    = SettingsDialog
    | BallotDialog String
    | InfoDialog
    | GethDialog
    | VerifyDialog
    | DebugDialog
    | NotFoundDialog
