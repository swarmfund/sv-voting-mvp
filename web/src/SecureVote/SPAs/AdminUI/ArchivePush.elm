module SecureVote.SPAs.AdminUI.ArchivePush exposing (..)

import Base64
import Http exposing (expectString, expectStringResponse, jsonBody)
import Json.Encode as E
import RemoteData
import SecureVote.SPAs.AdminUI.Fields exposing (..)
import SecureVote.SPAs.AdminUI.Model exposing (Model)
import SecureVote.SPAs.AdminUI.Msg exposing (Msg(SetLoadingField))


pushToArchive : Model -> Cmd (RemoteData.RemoteData String String)
pushToArchive model =
    let
        bodyVal =
            E.object
                [ ( "ballotBase64", E.string <| Base64.encode model.jsonBallot )
                , ( "assertSpecHash", E.string <| model.hash )
                ]
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-Api-Key" model.archivePushApiKey ]
        , url = model.archivePushURL
        , body = jsonBody bodyVal
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> RemoteData.sendRequest
        |> Cmd.map (RemoteData.mapError httpErrToStr)


httpErrToStr e =
    toString e
