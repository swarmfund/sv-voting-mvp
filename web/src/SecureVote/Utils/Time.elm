module SecureVote.Utils.Time exposing (..)

import SecureVote.SPAs.SwarmMVP.Model exposing (Model)
import Time exposing (Time)


readableTime : Int -> Model -> String
readableTime time model =
    let
        difference =
            toFloat <| abs <| 1000 * (time - model.now)
    in
    if Time.inSeconds difference < 120 && Time.inSeconds difference > 0 then
        (toString <| floor <| Time.inSeconds difference) ++ " seconds"
    else if Time.inMinutes difference < 120 && Time.inMinutes difference > 1 then
        (toString <| floor <| Time.inMinutes difference) ++ " minutes"
    else if Time.inHours difference < 48 && Time.inHours difference > 1 then
        (toString <| floor <| Time.inHours difference) ++ " hours"
    else if Time.inHours difference < 60 * 24 && Time.inHours difference > 24 then
        (toString <| floor <| Time.inHours difference / 24) ++ " days"
    else if Time.inHours difference < 730 * 24 && Time.inHours difference > 30 * 24 then
        (toString <| floor <| Time.inHours difference / 24 / 30) ++ " months"
    else if Time.inHours difference > 365 * 24 then
        (toString <| floor <| Time.inHours difference / 24 / 365) ++ " years"
    else
        "error reading number"
